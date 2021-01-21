let count_registers_in_window n reglobal regs =
  let regusage = Array.make 31 0 in
  let rec aux = function
    | 0, _ | _, [] -> ()
    | n, (None | Some 0) :: rem -> aux (n - 1, rem)
    | n, Some reg :: rem ->
      Array.set regusage (reg - 1) (regusage.(reg - 1) + 1);
      aux (n - 1, rem) in
  aux (n, regs);
  Array.to_list regusage
  |> List.map2 (fun (reg, c1) c2 -> reg, max c1 c2) reglobal
  (* List.map (fun (reg, c) ->
   *     if regusage.(reg - 1)
   * let _, reg, c = Array.fold_left
   * let _, reg, c = Array.fold_left (fun (i, reg, r_c) c ->
   *     if r_c < c then
   *       (i + 1, i, c)
   *     else
   *       (i + 1, reg, r_c)) (1, p_reg, p_c) regusage in
   * reg, c *)

let mkreglobal =
  let rec aux res = function
    | 0 -> res
    | n -> aux ((n, 0) :: res) (n - 1) in
  aux [] 31

let iterate_regs instrs =
  let rec aux reglobal instrs =
    if List.length instrs = 0 then
      reglobal
    else
      let reglobal = count_registers_in_window 8 reglobal instrs in
      aux reglobal (List.tl instrs) in
  aux mkreglobal (List.map Decoder.out_register instrs)
