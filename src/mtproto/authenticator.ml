open! Base
open Types

module TLG = TLGen.MTProto
module Crypto = Math.Crypto
module Bigint = Math.Bigint

module RsaManager = Crypto.Rsa.RsaManager

let generate_tmp_aes server_nonce new_nonce =
  let hash1 = Crypto.SHA1.digest (Cstruct.append new_nonce server_nonce) in
  let hash2 = Crypto.SHA1.digest (Cstruct.append server_nonce new_nonce) in
  let hash3 = Crypto.SHA1.digest (Cstruct.append new_nonce new_nonce) in

  let tmp_aes_key = Cstruct.create_unsafe 32 in
  Cstruct.blit hash1 0 tmp_aes_key 0 20;
  Cstruct.blit hash2 0 tmp_aes_key 20 12;

  let tmp_aes_iv = Cstruct.create_unsafe 32 in
  Cstruct.blit hash2 12 tmp_aes_iv 0 8;
  Cstruct.blit hash3 0 tmp_aes_iv 8 20;
  Cstruct.blit new_nonce 0 tmp_aes_iv 28 4;

  (tmp_aes_key, tmp_aes_iv)

(* type auth_error = [
  | `InvalidNonce
  | `InvalidServerNonce
  | `DhGenRetry
  | `DhGenFail
] *)

exception AuthenticationError of string

let authenticate
  (type sender_t)
  (module Sender : MTProtoPlainObjSender with type t = sender_t)
  (t: sender_t)
  (rsa: RsaManager.t)
=
  let open Sender in

  let nonce = Crypto.SecureRand.rand_cs 16 in

  (* Logger.dump "nonce" nonce; *)

  let%lwt (C_resPQ ({ server_nonce; _ } as res_pq)) =
    invoke_unencrypted_obj t (module TLG.C_req_pq_multi) { nonce } in

  if Cstruct.equal res_pq.nonce nonce |> not then
    raise @@ AuthenticationError "1: Invalid nonce from server";

  (* Logger.dump "server_nonce" res_pq.server_nonce; *)

  let pq = Cstruct.BE.get_uint64 res_pq.pq 0 in
  let (p, q) = Math.Factorization.pq_prime pq in

  let p_bytes = Cstruct.create_unsafe 4 in
  Cstruct.BE.set_uint32 p_bytes 0 (Int32.of_int64_exn p);
  let q_bytes = Cstruct.create_unsafe 4 in
  Cstruct.BE.set_uint32 q_bytes 0 (Int32.of_int64_exn q);

  (* int256 (32 bytes) *)
  let new_nonce = Crypto.SecureRand.rand_cs 32 in

  let p_q_inner_data = TL.Encoder.encode TLG.C_p_q_inner_data.encode_boxed {
    pq = res_pq.pq;
    p = p_bytes;
    q = q_bytes;
    nonce;
    server_nonce;
    new_nonce;
  } |> TL.Encoder.to_cstruct in

  let data_with_hash = Cstruct.create 255 in
  Cstruct.blit (Crypto.SHA1.digest p_q_inner_data) 0 data_with_hash 0 20;
  Cstruct.blit p_q_inner_data 0 data_with_hash 20 (Cstruct.len p_q_inner_data);

  let (C_vector fingerprints) = res_pq.server_public_key_fingerprints in
  let (rsa_key, finger) = RsaManager.find_by_fingerprints rsa fingerprints in

  let encrypted_data = RsaManager.encrypt ~key:rsa_key data_with_hash in

  let%lwt dh_params = invoke_unencrypted_obj t (module TLG.C_req_DH_params) {
    nonce;
    server_nonce;
    p = p_bytes;
    q = q_bytes;
    public_key_fingerprint = finger;
    encrypted_data;
  } in

  match dh_params with
  | C_server_DH_params_ok params ->
    begin
      Caml.print_endline "server_DH_params_ok";

      if Cstruct.equal params.nonce nonce |> not then
        raise @@ AuthenticationError "2: Invalid nonce from server";

      if Cstruct.equal params.server_nonce server_nonce |> not then
        raise @@ AuthenticationError "2: Invalid server_nonce from server";

      let (tmp_key, tmp_iv) = generate_tmp_aes server_nonce new_nonce in

      let decrypted_answer_with_hash =
        Crypto.IGE.decrypt params.encrypted_answer tmp_key tmp_iv in
      let given_hash = Cstruct.sub decrypted_answer_with_hash 0 20 in
      let decrypted_answer = Cstruct.shift decrypted_answer_with_hash 20 in
      let (C_server_DH_inner_data server_dh_inner) =
        TLG.Server_DH_inner_data.decode (TL.Decoder.of_cstruct decrypted_answer) in

      (* Check hash *)
      let calc_hash =
        TL.Encoder.encode TLG.C_server_DH_inner_data.encode_boxed server_dh_inner
          |> TL.Encoder.to_cstruct |> Crypto.SHA1.digest in
      if Cstruct.equal calc_hash given_hash |> not then
        raise @@ AuthenticationError "3: Invalid hash";

      if Cstruct.equal server_dh_inner.nonce nonce |> not then
        raise @@ AuthenticationError "3: Invalid nonce from server";

      if Cstruct.equal server_dh_inner.server_nonce server_nonce |> not then
        raise @@ AuthenticationError "3: Invalid server_nonce from server";

      (* Logger.dump "dh_prime" server_dh_inner.dh_prime; *)
      (* Caml.print_endline @@ Printf.sprintf
        "server_time %d\n" server_dh_inner.server_time; *)

      let current_time = Platform.get_current_time () |> Float.to_int in
      let time_offset = server_dh_inner.server_time - current_time in

      let dh_prime = Bigint.of_cstruct_be server_dh_inner.dh_prime in
      let b = Bigint.of_cstruct_be @@ Crypto.SecureRand.rand_cs 256 in
      let g = Bigint.of_int server_dh_inner.g in
      let g_a = Bigint.of_cstruct_be server_dh_inner.g_a in

      (* pow(g, b) mod dh_prime *)
      let g_b = Bigint.powm g b dh_prime in
      let g_b_cs = Bigint.to_cstruct_be g_b in

      let g_a_b = Bigint.powm g_a b dh_prime in
      let auth_key = Bigint.to_cstruct_be g_a_b in

      Caml.print_endline @@ Printf.sprintf
        "Auth key created. Auth_key len: %d. Time_offset: %d."
        (Cstruct.len auth_key) time_offset;

      (* TODO: Checks from https://core.telegram.org/mtproto/security_guidelines *)

      if not Bigint.(g > one && g_a > one && g_b > one) then
        raise @@ AuthenticationError "3: Should be greater than 1";

      let d = Bigint.(dh_prime - one) in
      if not Bigint.(g < d && g_a < d && g_b < d) then
        raise @@ AuthenticationError "3: Should be less than dh_prime - 1";

      (* TODO: check that g_a and g_b are between 2^{2048-64} and dh_prime - 2^{2048-64} *)

      let client_dh_inner_data =
        TL.Encoder.encode TLG.C_client_DH_inner_data.encode_boxed {
          nonce;
          server_nonce;
          retry_id = 0L; (* TODO: *)
          g_b = g_b_cs;
        } |> TL.Encoder.to_cstruct in

      let len = Cstruct.len client_dh_inner_data in
      let len_with_hash = len + 20 in

      let data_with_hash = Cstruct.create (len_with_hash + (16 - len_with_hash % 16)) in
      Cstruct.blit (Crypto.SHA1.digest client_dh_inner_data) 0 data_with_hash 0 20;
      Cstruct.blit client_dh_inner_data 0 data_with_hash 20 len;
      let encrypted_data = Crypto.IGE.encrypt data_with_hash tmp_key tmp_iv in

      (* Logger.dump "client_dh_inner_data" client_dh_inner_data;
      Logger.dump "dh data_with_hash" data_with_hash; *)

      let%lwt dh_answer = invoke_unencrypted_obj t (module TLG.C_set_client_DH_params)
        { nonce; server_nonce; encrypted_data; } in

      match dh_answer with
      | C_dh_gen_ok dh_gen -> begin
        Caml.print_endline "dh_gen_ok";

        if Cstruct.equal dh_gen.nonce nonce |> not then
          raise @@ AuthenticationError "end: Invalid nonce from server";

        if Cstruct.equal dh_gen.server_nonce server_nonce |> not then
          raise @@ AuthenticationError "end: Invalid server_nonce from server";

        let auth_key_aux_hash = Crypto.SHA1.digest auth_key in
        let auth_key_aux_hash = Cstruct.sub auth_key_aux_hash 0 8 in

        let new_nonce_hash = Cstruct.create_unsafe 41 in
        Cstruct.blit new_nonce 0 new_nonce_hash 0 32;
        Cstruct.set_char new_nonce_hash 32 '\001';
        Cstruct.blit auth_key_aux_hash 0 new_nonce_hash 33 8;
        let new_nonce_hash = Crypto.SHA1.digest new_nonce_hash in
        let new_nonce_hash = Cstruct.sub new_nonce_hash 4 16 in

        if Cstruct.equal new_nonce_hash dh_gen.new_nonce_hash1 |> not then
          raise @@ AuthenticationError "end: Invalid new_nonce_hash";

        let server_salt = Int64.(
          (Cstruct.LE.get_uint64 new_nonce 0)
          lxor (Cstruct.LE.get_uint64 server_nonce 0)
        ) in

        Lwt.return (auth_key, server_salt, time_offset)
      end
      | C_dh_gen_retry _ -> raise @@ AuthenticationError "dh_gen_retry" (* TODO: *)
      | C_dh_gen_fail _ -> raise @@ AuthenticationError "dh_gen_fail"
    end
  | C_server_DH_params_fail _ ->
    raise @@ AuthenticationError "server_DH_params_fail"