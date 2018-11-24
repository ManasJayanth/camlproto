(* AUTOGENERATED *)

open! Base
open TL
open Types
open Builtin

module C_true = struct
  type t = C
  include MakeConstrR(struct
    type nonrec t = t
    let magic = 0x3fedd339l
    let encode _ _ =
      ()
    ;;
    let decode _ =
      C
  end)
end

module True = struct
  type t =
    | C_true of C_true.t
  include MakeTLTypeR(struct
    type nonrec t = t
    let encode enc t = match t with
      | C_true x -> C_true.encode_boxed enc x
    let decode dec =
      let magic = Decoder.read_int32_le dec in
      let open Int32 in
      match magic with
      | x when x = C_true.magic -> C_true (C_true.decode dec)
      | x -> raise @@ DeserializationError ("Invalid True magic " ^ to_string x)
  end)
end

(* --- Functions --- *)

module C_invokeAfterMsgs (X: TLFunc) = struct
  module VectorTLLong = Vector(TLLong)
  module VectorX = Vector(X)
  type t = {
    msg_ids: VectorTLLong.t;
    query: X.t;
  }
  include MakeFuncR(struct
    type nonrec t = t
    module ResultM = VectorX
    let magic = 0x3dc4b4f0l
    let encode enc t =
      VectorTLLong.encode enc t.msg_ids;
      X.encode_boxed enc t.query;
    ;;
    let decode dec =
      let msg_ids = VectorTLLong.decode dec in
      let query = X.decode dec in
      { msg_ids; query }
  end)
end

module C_invokeAfterMsgs2 (X: TLFunc) = struct
  module VectorTLLong = Vector(TLLong)
  type t = {
    msg_ids: VectorTLLong.t;
    query: X.t;
  }
  include MakeFuncR(struct
    type nonrec t = t
    module ResultM = X.ResultM
    let magic = 0x3dc4b4f5l
    let encode enc t =
      VectorTLLong.encode enc t.msg_ids;
      X.encode_boxed enc t.query;
    ;;
    let decode dec =
      let msg_ids = VectorTLLong.decode dec in
      let query = X.decode dec in
      { msg_ids; query }
  end)
end

module C_flagsTest = struct
  type t = {
    w: TLInt.t;
    a: TLString.t option;
    w2: TLString.t;
    b: C_true.t option;
    bb: C_true.t option;
  }
  include MakeFuncR(struct
    type nonrec t = t
    module ResultM = TLBool
    let magic = 0x10000001l
    let encode enc t =
      let flags = ref 0l in
      begin [@warning "-33"] Int32.(
        match t.a with
          Some _ -> flags := !flags lor (1l lsl 0) | None -> ();
        match t.b with
          Some _ -> flags := !flags lor (1l lsl 1) | None -> ();
        match t.bb with
          Some _ -> flags := !flags lor (1l lsl 22) | None -> ();
      ) end;
      TLNat.encode enc !flags;
      TLInt.encode enc t.w;
      match t.a with Some x -> TLString.encode enc x | None -> ();
      TLString.encode enc t.w2;
      match t.b with Some x -> C_true.encode enc x | None -> ();
      match t.bb with Some x -> C_true.encode enc x | None -> ();
    ;;
    let decode dec =
      let [@warning "-26"] flags = TLNat.decode dec in
      let w = TLInt.decode dec in
      let a =
        if Int32.(flags land (1l lsl 0) <> 0l) then
          Some (TLString.decode dec)
        else None
      in
      let w2 = TLString.decode dec in
      let b =
        if Int32.(flags land (1l lsl 1) <> 0l) then
          Some (C_true.decode dec)
        else None
      in
      let bb =
        if Int32.(flags land (1l lsl 22) <> 0l) then
          Some (C_true.decode dec)
        else None
      in
      { w; a; w2; b; bb }
  end)
end
