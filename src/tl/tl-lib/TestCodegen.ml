open! Base
open Translate

let t str_in =
  let (errs, str_out) = translate str_in in
  List.iter errs ~f:(fun e -> Caml.print_endline @@ Err.show e);
  Caml.print_endline "---";
  Caml.print_endline str_out

let%expect_test "basic" =
  t "
    a1 = A;
    a2 = A;
    a3#50 = A;
    b1 v:A = B;
    ---functions---
    fn1 a:b1 = A;
    fn2_vector_arg a:(Vector b1) = A;
    fn3_no_args = A;
    fnPoly1 {X:Type} query:!X = X;
    fnPoly2 {X:Type} query2:!X = B;
    fnVectorResult1 {X:Type} query3:!X = Vector<X>;
    fnFlags flags:# a:flags.3?A somevar:# b:somevar.5?(vector A) a2:A = A;

    ---types---

    // recursive types

    entityBoxed ent2:Ent = Ent;
    entityNull = Ent;

    recA b:RecB = RecA;
    recB a:RecA = RecB;
  ";
  [%expect {|
    ---
    (* AUTOGENERATED *)

    open! TLRuntime
    open! Types
    open! Builtin
    open! I32

    [@@@warning "-26-27-33"]

    module rec TL_recA : sig
      type t = {
        b : TLT_RecB.t;
      }
      include TLConstr with type t := t
    end = struct
      type t = {
        b : TLT_RecB.t;
      }
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0xdd718d07l
        let encode enc t =
          TLT_RecB.encode enc t.b;
          ()
        ;;
        let decode dec =
          let b = TLT_RecB.decode dec in
          { b }
      end)
    end

    and TLT_RecB : sig
      type [@unboxed] t =
        | TL_recB of TL_recB.t
      include TLType with type t := t
    end = struct
      type [@unboxed] t =
        | TL_recB of TL_recB.t
      include MakeType(struct
        type nonrec t = t
        let encode enc t = match t with
          | TL_recB x -> TL_recB.encode_boxed enc x
        ;;
        let decode dec =
          let magic = Decoder.read_int32_le dec in
          match magic with
          | 0x6640dde9l -> TL_recB (TL_recB.decode dec)
          | x -> raise (DeserializationError x)
      end)
    end

    and TL_recB : sig
      type t = {
        a : TLT_RecA.t;
      }
      include TLConstr with type t := t
    end = struct
      type t = {
        a : TLT_RecA.t;
      }
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0x6640dde9l
        let encode enc t =
          TLT_RecA.encode enc t.a;
          ()
        ;;
        let decode dec =
          let a = TLT_RecA.decode dec in
          { a }
      end)
    end

    and TLT_RecA : sig
      type [@unboxed] t =
        | TL_recA of TL_recA.t
      include TLType with type t := t
    end = struct
      type [@unboxed] t =
        | TL_recA of TL_recA.t
      include MakeType(struct
        type nonrec t = t
        let encode enc t = match t with
          | TL_recA x -> TL_recA.encode_boxed enc x
        ;;
        let decode dec =
          let magic = Decoder.read_int32_le dec in
          match magic with
          | 0xdd718d07l -> TL_recA (TL_recA.decode dec)
          | x -> raise (DeserializationError x)
      end)
    end

    module TL_entityNull : sig
      type t = E
      include TLConstr with type t := t
    end = struct
      type t = E
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0x0d1534b3l
        let encode enc t =
          ()
        ;;
        let decode dec =
          E
      end)
    end

    module rec TL_entityBoxed : sig
      type t = {
        ent2 : TLT_Ent.t;
      }
      include TLConstr with type t := t
    end = struct
      type t = {
        ent2 : TLT_Ent.t;
      }
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0x4060119el
        let encode enc t =
          TLT_Ent.encode enc t.ent2;
          ()
        ;;
        let decode dec =
          let ent2 = TLT_Ent.decode dec in
          { ent2 }
      end)
    end

    and TLT_Ent : sig
      type t =
        | TL_entityBoxed of TL_entityBoxed.t
        | TL_entityNull of TL_entityNull.t
      include TLType with type t := t
    end = struct
      type t =
        | TL_entityBoxed of TL_entityBoxed.t
        | TL_entityNull of TL_entityNull.t
      include MakeType(struct
        type nonrec t = t
        let encode enc t = match t with
          | TL_entityBoxed x -> TL_entityBoxed.encode_boxed enc x
          | TL_entityNull x -> TL_entityNull.encode_boxed enc x
        ;;
        let decode dec =
          let magic = Decoder.read_int32_le dec in
          match magic with
          | 0x4060119el -> TL_entityBoxed (TL_entityBoxed.decode dec)
          | 0x0d1534b3l -> TL_entityNull (TL_entityNull.decode dec)
          | x -> raise (DeserializationError x)
      end)
    end

    module TL_a3 : sig
      type t = E
      include TLConstr with type t := t
    end = struct
      type t = E
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0x00000050l
        let encode enc t =
          ()
        ;;
        let decode dec =
          E
      end)
    end

    module TL_a2 : sig
      type t = E
      include TLConstr with type t := t
    end = struct
      type t = E
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0xb1551ae3l
        let encode enc t =
          ()
        ;;
        let decode dec =
          E
      end)
    end

    module TL_a1 : sig
      type t = E
      include TLConstr with type t := t
    end = struct
      type t = E
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0xf6f56033l
        let encode enc t =
          ()
        ;;
        let decode dec =
          E
      end)
    end

    module TLT_A : sig
      type t =
        | TL_a1 of TL_a1.t
        | TL_a2 of TL_a2.t
        | TL_a3 of TL_a3.t
      include TLType with type t := t
    end = struct
      type t =
        | TL_a1 of TL_a1.t
        | TL_a2 of TL_a2.t
        | TL_a3 of TL_a3.t
      include MakeType(struct
        type nonrec t = t
        let encode enc t = match t with
          | TL_a1 x -> TL_a1.encode_boxed enc x
          | TL_a2 x -> TL_a2.encode_boxed enc x
          | TL_a3 x -> TL_a3.encode_boxed enc x
        ;;
        let decode dec =
          let magic = Decoder.read_int32_le dec in
          match magic with
          | 0xf6f56033l -> TL_a1 (TL_a1.decode dec)
          | 0xb1551ae3l -> TL_a2 (TL_a2.decode dec)
          | 0x00000050l -> TL_a3 (TL_a3.decode dec)
          | x -> raise (DeserializationError x)
      end)
    end

    module TL_b1 : sig
      type t = {
        v : TLT_A.t;
      }
      include TLConstr with type t := t
    end = struct
      type t = {
        v : TLT_A.t;
      }
      include MakeConstr(struct
        type nonrec t = t
        let magic () = 0x38b9673cl
        let encode enc t =
          TLT_A.encode enc t.v;
          ()
        ;;
        let decode dec =
          let v = TLT_A.decode dec in
          { v }
      end)
    end

    module TLT_B : sig
      type [@unboxed] t =
        | TL_b1 of TL_b1.t
      include TLType with type t := t
    end = struct
      type [@unboxed] t =
        | TL_b1 of TL_b1.t
      include MakeType(struct
        type nonrec t = t
        let encode enc t = match t with
          | TL_b1 x -> TL_b1.encode_boxed enc x
        ;;
        let decode dec =
          let magic = Decoder.read_int32_le dec in
          match magic with
          | 0x38b9673cl -> TL_b1 (TL_b1.decode dec)
          | x -> raise (DeserializationError x)
      end)
    end

    (* -- Functions -- *)

    module TL_fnVectorResult1 (X : TLFunc) : sig
      type t = {
        query3 : X.t;
      }
      include TLFunc with type t := t and module ResultM = TLT_Vector(X)
    end = struct
      module TLT_VectorX = TLT_Vector (X)
      type t = {
        query3 : X.t;
      }
      module ResultM = TLT_VectorX
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0xae3ef4c4l
        let encode enc t =
          X.encode enc t.query3;
          ()
        ;;
        let decode dec =
          let query3 = X.decode dec in
          { query3 }
      end)
    end

    module TL_fnPoly2 (X : TLFunc) : sig
      type t = {
        query2 : X.t;
      }
      include TLFunc with type t := t and module ResultM = TLT_B
    end = struct
      type t = {
        query2 : X.t;
      }
      module ResultM = TLT_B
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0x7c29a776l
        let encode enc t =
          X.encode enc t.query2;
          ()
        ;;
        let decode dec =
          let query2 = X.decode dec in
          { query2 }
      end)
    end

    module TL_fnPoly1 (X : TLFunc) : sig
      type t = {
        query : X.t;
      }
      include TLFunc with type t := t and module ResultM = X.ResultM
    end = struct
      type t = {
        query : X.t;
      }
      module ResultM = X.ResultM
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0x34c7208al
        let encode enc t =
          X.encode enc t.query;
          ()
        ;;
        let decode dec =
          let query = X.decode dec in
          { query }
      end)
    end

    module TL_fnFlags : sig
      type t = {
        a : TLT_A.t option;
        b : TL_vector(TLT_A).t option;
        a2 : TLT_A.t;
      }
      include TLFunc with type t := t and module ResultM = TLT_A
    end = struct
      module TL_vectorTLT_A = TL_vector (TLT_A)
      type t = {
        a : TLT_A.t option;
        b : TL_vectorTLT_A.t option;
        a2 : TLT_A.t;
      }
      module ResultM = TLT_A
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0xe3dcf7f8l
        let encode enc t =
          let flags = ref 0l in
          match t.a with Some _ -> flags := !flags lor (1l lsl 3) | None -> ();
          TL_nat.encode enc !flags;
          match t.a with Some x -> TLT_A.encode enc x | None -> ();
          let somevar = ref 0l in
          match t.b with Some _ -> somevar := !somevar lor (1l lsl 5) | None -> ();
          TL_nat.encode enc !somevar;
          match t.b with Some x -> TL_vectorTLT_A.encode enc x | None -> ();
          TLT_A.encode enc t.a2;
          ()
        ;;
        let decode dec =
          let flags = TL_nat.decode dec in
          let a =
            if flags land (1l lsl 3) <> 0l
            then Some (TLT_A.decode dec)
            else None
          in
          let somevar = TL_nat.decode dec in
          let b =
            if somevar land (1l lsl 5) <> 0l
            then Some (TL_vectorTLT_A.decode dec)
            else None
          in
          let a2 = TLT_A.decode dec in
          { a; b; a2 }
      end)
    end

    module TL_fn3_no_args : sig
      type t = E
      include TLFunc with type t := t and module ResultM = TLT_A
    end = struct
      type t = E
      module ResultM = TLT_A
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0x5f432824l
        let encode enc t =
          ()
        ;;
        let decode dec =
          E
      end)
    end

    module TL_fn2_vector_arg : sig
      type t = {
        a : TLT_Vector(TL_b1).t;
      }
      include TLFunc with type t := t and module ResultM = TLT_A
    end = struct
      module TLT_VectorTL_b1 = TLT_Vector (TL_b1)
      type t = {
        a : TLT_VectorTL_b1.t;
      }
      module ResultM = TLT_A
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0x21fd7c7al
        let encode enc t =
          TLT_VectorTL_b1.encode enc t.a;
          ()
        ;;
        let decode dec =
          let a = TLT_VectorTL_b1.decode dec in
          { a }
      end)
    end

    module TL_fn1 : sig
      type t = {
        a : TL_b1.t;
      }
      include TLFunc with type t := t and module ResultM = TLT_A
    end = struct
      type t = {
        a : TL_b1.t;
      }
      module ResultM = TLT_A
      include MakeFunc(struct
        type nonrec t = t
        module ResultM = ResultM
        let magic () = 0x6a29301bl
        let encode enc t =
          TL_b1.encode enc t.a;
          ()
        ;;
        let decode dec =
          let a = TL_b1.decode dec in
          { a }
      end)
    end |}]