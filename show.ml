let show (type a) ~(t: a Type.ttype) (x: a) : unit =
  let rec show (t : Type.stype) (x : Obj.t) : unit =
    match t with
    | Int -> print_int (Obj.obj x)
    | String -> Printf.printf "%S" (Obj.obj x)
    | List t ->
      print_char '[';
      List.iteri (fun i x -> if i > 0 then print_string "; "; show t x) (Obj.obj x);
      print_char ']'
    | Tuple tl ->
      print_char '(';
      List.iteri (fun i t -> if i > 0 then print_string ", "; show t (Obj.field x i)) tl;
      print_char ')'
    | Sum cstrs ->
      let cst, noncst = List.partition (function (_, []) -> true | (_, _ :: _) -> false) cstrs in
      if Obj.is_int x then begin
        let name, _ = List.nth cst (Obj.obj x) in
        print_string name
      end else begin
        let name, tl = List.nth noncst (Obj.tag x) in
        print_string name; print_char ' '; show (Tuple tl) x
      end
  in
  show (Type.stype_of_ttype t) (Obj.repr x)
