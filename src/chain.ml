let count_registers_in_window n reglobal addr regs =
  let regusage = Array.make 31 0 in
  let rec aux = function
    | 0, _ | _, [] -> ()
    | n, (None | Some 0) :: rem -> aux (n - 1, rem)
    | n, (Some reg) :: rem ->
      Array.set regusage (reg - 1) (regusage.(reg - 1) + 1);
      aux (n - 1, rem) in
  aux (n, regs);
  Array.to_list regusage
  |> List.map2 (fun (reg, addr1, c1) c2 ->
      if c1 > c2 then
        reg, addr1, c1
      else
        reg, addr, c2) reglobal

let mkreglobal =
  let rec aux res = function
    | 0 -> res
    | n -> aux ((n, 0, 0) :: res) (n - 1) in
  aux [] 31

let iterate_regs window_size (addrs, instrs) =
  let rec aux reglobal (addrs, instrs) =
    if List.length instrs = 0 then
      reglobal
    else
      let addr = List.hd addrs in
      let reglobal = count_registers_in_window window_size reglobal addr instrs in
      aux reglobal List.(tl addrs, tl instrs) in
  aux mkreglobal (addrs, List.map Decoder.out_register instrs)
