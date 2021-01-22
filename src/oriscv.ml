let window_size = ref 8

let params = [("-w", Arg.Set_int window_size, "Sets the window size")]
let usage = "oriscv is a simple RV32I instruction statistics generator"

let () =
  let filename = ref None in
  Arg.parse params (fun f -> filename := Some f) usage;
  match !filename with
  | None ->
    Arg.usage params usage;
    exit 1
  | Some filename ->
    let ic = Scanf.Scanning.open_in filename in
    let rec read_instr_stream addrs raw_instrs =
      try
        let addr, num = Scanf.bscanf ic "%x: %x\n" (fun addr num -> addr, num) in
        read_instr_stream (addr :: addrs) (num :: raw_instrs)
      with End_of_file ->
        Scanf.Scanning.close_in ic;
        addrs, raw_instrs in
    let _addrs, raw_instrs = read_instr_stream [] [] in
    let instrs = List.fold_left (fun instrs raw ->
        (Decoder.decode_instr raw) :: instrs) [] raw_instrs in
    Patterns.find_patterns instrs;
    let chains = Chain.iterate_regs instrs in
    print_endline "Longest chains:";
    List.iter (fun (reg, c) ->
        Printf.printf "Register %s: %d\n" (Decoder.register_to_abi reg) c) chains
