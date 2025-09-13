let show (type a) ~(t: a Type.ttype) (x: a) : unit =
  let x = Obj.repr x in
  match Type.stype_of_ttype t with
  | Int -> print_int (Obj.obj x)
  | String -> Printf.printf "%S" (Obj.obj x)
