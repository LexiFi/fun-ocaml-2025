let show (type a) ~(t: a Type.ttype) (x: a) : unit =
  let rec show (t : Type.stype) (x : Obj.t) : unit =
    match t with
    | Int -> print_int (Obj.obj x)
    | String -> Printf.printf "%S" (Obj.obj x)
    | List t ->
      print_char '[';
      List.iteri (fun i x -> if i > 0 then print_string "; "; show t x) (Obj.obj x);
      print_char ']'
  in
  show (Type.stype_of_ttype t) (Obj.repr x)
