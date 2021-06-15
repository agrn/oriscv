let window_size = ref 8
let decode_instr = ref false

let params = [("-w", Arg.Set_int window_size, "Sets the window size");
              ("-d", Arg.Set decode_instr, "Decode a single instruction")]
let usage = "oriscv is a simple RV32I instruction statistics generator"

let read_instr_stream filename line_parser =
  let ic = Scanf.Scanning.open_in filename in
  let rec read_instr_stream data =
    try
      read_instr_stream (line_parser ic data)
    with End_of_file ->
      Scanf.Scanning.close_in ic;
      data in
  read_instr_stream

let () =
  let rem = ref None in
  Arg.parse params (fun f -> rem := Some f) usage;
  match !decode_instr, !rem with
  | _, None ->
    Arg.usage params usage;
    exit 1
  | false, Some filename ->
    let addr_and_instr_parser ic (addrs, instrs) =
      Scanf.bscanf ic "%x: %x\n" (fun addr instr -> addr :: addrs, instr :: instrs) in
    let addrs, instrs = read_instr_stream filename addr_and_instr_parser ([], []) in
    let addrs = List.rev addrs in
    let instrs = List.fold_left (fun instrs raw ->
        (Decoder.decode_instr raw) :: instrs) [] instrs in
    Patterns.find_patterns instrs;
    let chains = Chain.iterate_regs (!window_size) (addrs, instrs) in
    print_endline "Longest chains:";
    List.iter (fun (reg, addr, c) ->
        Printf.printf "Register %s: %d (0x%x)\n" (Decoder.register_to_abi reg) c addr) chains
  | true, Some instr ->
    let instr = int_of_string ("0x" ^ instr) in
    Decoder.decode_instr instr
    |> Decoder.print_instr
    |> print_endline
