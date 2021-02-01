let initial_window n addr regs =
  let regusage = Array.make 31 0 in
  let rec aux = function
    | 0, _ | _, [] -> ()
    | n, (None | Some 0) :: rem -> aux (n - 1, rem)
    | n, (Some reg) :: rem ->
      Array.set regusage (reg - 1) (regusage.(reg - 1) + 1);
      aux (n - 1, rem) in
  aux (n, regs);
  let reglobal = Array.map (fun c -> addr, c) regusage in
  regusage, reglobal

let next_window reglobal regusage addr prevreg nextreg =
  begin
    match prevreg with
    | None | Some 0 -> ()
    | Some reg ->
      Array.set regusage (reg - 1) (regusage.(reg - 1) - 1)
  end;
  match nextreg with
  | None | Some 0 -> ()
  | Some reg ->
    let c = regusage.(reg - 1) + 1 in
    Array.set regusage (reg - 1) c;
    let _, prev = reglobal.(reg - 1) in
    if prev < c then
      Array.set reglobal (reg - 1) (addr, c)

let rec remove_first_nth n l =
  match n, l with
  | 0, res | _, ([] as res) -> res
  | n, (_ :: res) -> remove_first_nth (n - 1) res

let iterate_regs window_size (addrs, instrs) =
  let rec aux reglobal regusage addrs instrs nextinstrs =
    match addrs, instrs, nextinstrs with
    | addr :: addrs, prevreg :: instrs, nextreg :: nextinstrs ->
      next_window reglobal regusage addr prevreg nextreg;
      aux reglobal regusage addrs instrs nextinstrs
    | _, _, _ ->
      Array.to_list reglobal
      |> List.mapi (fun i (addr, c) -> (i + 1, addr, c)) in
  let instrs = List.map Decoder.out_register instrs in
  let nextinstrs = remove_first_nth window_size instrs in
  let regusage, reglobal = initial_window window_size (List.hd addrs) instrs in
  aux reglobal regusage (List.tl addrs) instrs nextinstrs
