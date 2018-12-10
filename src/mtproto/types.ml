open! Base
open Math

module type MTProtoStorage = sig (* TODO: currently not used *)
  type t
  val create: unit -> t
  val set_auth_key: t -> Cstruct.t -> unit
  val get_auth_key: t -> Cstruct.t
end

module type MTProtoPlainObjSender = sig
  open TL.Types
  type t
  val send_unencrypted_obj
    : t -> (module TLFunc with type t = 'a) -> 'a -> unit Lwt.t
  val receive_unencrypted_obj
    : t -> (module TLObject with type t = 'a) -> 'a Lwt.t
  val invoke_unencrypted_obj
    : t -> (module TLFunc with type t = 'a and type ResultM.t = 'b) -> 'a -> 'b Lwt.t
end

module type MTProtoPlainSender = sig
  type t
  val send_unencrypted: t -> Cstruct.t -> unit Lwt.t
  val receive_unencrypted: t -> Cstruct.t Lwt.t
  include MTProtoPlainObjSender with type t := t
end

module type MTProtoClient = sig
  open TL.Types

  type t

  exception MTPError of string

  exception RpcError of int * string
  (** [RpcError (error_code, error_message)] *)

  include MTProtoPlainSender with type t := t

  val create: ?rsa:Crypto.Rsa.RsaManager.t -> unit -> t Lwt.t

  val reset_state: t -> unit

  val do_authentication: t -> unit Lwt.t

  val send_encrypted_obj
    : t
    -> ?msg_id:int64
    -> ?content_related:bool
    -> (module TLFunc with type t = 'a)
    -> 'a
    -> unit Lwt.t

  val recv_loop: t -> unit Lwt.t

  val invoke
    : t
    -> ?content_related:bool
    -> (module TLFunc with type t = 'a and type ResultM.t = 'b)
    -> 'a
    -> 'b Lwt.t
end
