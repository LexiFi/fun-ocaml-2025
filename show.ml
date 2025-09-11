let show (type a) ~(t: a Type.ttype) (x: a) : unit =
  match Type.equal [%t: int] t with
  | Some Equal -> print_int x
  | None ->
  match Type.equal [%t: string] t with
  | Some Equal -> print_string x
  | None -> print_string "<unsupported>"
