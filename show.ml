let rec show : type a. t: a Type.ttype -> a -> unit = fun ~t x ->
  match Type.equal [%t: int] t with
  | Some Equal -> print_int x
  | None ->
  match Type.equal [%t: string] t with
  | Some Equal -> print_string x
  | None ->
  match Type.is_list t with
  | Some (Is_list (t, Equal)) ->
    print_string "[";
    List.iteri (fun i x ->
      if i > 0 then print_string "; ";
      show ~t x
    ) x;
    print_string "]"
  | None ->
  print_string "<unsupported>"
