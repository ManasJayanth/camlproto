open! Base

let pp_header =
  let pf = Stdlib.Format.fprintf in
  fun ppf (level, src, header) -> match header with
    | None -> pf ppf "[%a][%s] " Logs.pp_level level (Logs.Src.name src)
    | Some h -> pf ppf "[%s][%s] " h (Logs.Src.name src)

let reporter ?(app = Stdlib.Format.std_formatter) ?(dst = Stdlib.Format.err_formatter) () =
  let report src level ~over k msgf =
    (* Stdlib.print_endline "report"; *)
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
      let ppf = match level with Logs.App -> app | _ -> dst in
      Stdlib.(Format.kfprintf
        k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, src, header))
  in
  { Logs. report }
  (* Logs.{ report } *)
