let () =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "Usage: %s <instruction file>\n" Sys.argv.(0);
      exit 1
    end
  else
    let ic = open_in (Sys.argv.(1)) in
    let rec read_instr_stream raw_instrs =
      try
        let line = input_line ic in
        let num = int_of_string_opt ("0x" ^ line) in
        match num with
        | None -> read_instr_stream raw_instrs
        | Some raw -> read_instr_stream (raw :: raw_instrs)
      with End_of_file ->
        close_in ic;
        raw_instrs in
    let raw_instrs = read_instr_stream [] in
    let _instrs = List.fold_left (fun res i -> (Decoder.decode_instr i) :: res) [] raw_instrs in
    ()
