type stats = {
  branch : int;
  jumps : int;
  lla : int;
  la : int;
  far_calls : int;
  li : int;
  load_global : int;
  store_global : int;
}

let init_stats = {
  branch = 0;
  jumps = 0;
  lla = 0;
  la = 0;
  far_calls = 0;
  li = 0;
  load_global = 0;
  store_global = 0;
}

let count_instr_patterns instrs =
  let rec aux st = function
    | Decoder.(Utype (Auipc, rs1, _) :: Itype (Addi, _, rs2, _) :: rem) when rs1 = rs2 ->
      aux {st with lla = st.lla + 1} rem
    | Decoder.(Utype (Auipc, rs1, _) :: Itype (Jalr, _, rs2, _) :: rem) when rs1 = rs2 ->
      aux {st with far_calls = st.far_calls + 1; jumps = st.jumps + 1} rem
    | Decoder.(Utype (Lui, rs1, _) :: Itype (Addi, _, rs2, _) :: rem) when rs1 = rs2 ->
      aux {st with li = st.li + 1} rem
    | Decoder.Btype _ :: rem ->
      aux {st with branch = st.branch + 1} rem
    | Decoder.Jtype _ :: rem | Decoder.Itype (Jalr, _, _, _) :: rem ->
      aux {st with jumps = st.jumps + 1} rem
    | _ :: rem -> aux st rem
    | [] -> st in
  aux init_stats instrs

let find_patterns instrs =
  let count = List.length instrs in
  let st = count_instr_patterns instrs in
  let percent sub = ((float_of_int sub) /. (float_of_int count)) *. 100. in
  let print_st sub name =
    Printf.printf "%d %s (%f%%)\n" sub name (percent sub) in
  Printf.printf "%d instructions.\n" count;
  print_st st.branch "branches";
  print_st st.jumps "jumps";
  print_st st.lla "lla";
  print_st st.la "la";
  print_st st.far_calls "far calls";
  print_st st.li "li";
  print_st st.load_global "load global";
  print_st st.store_global "store global"
