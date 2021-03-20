type rinstr =
  | Add
  | Sub
  | Sll
  | Slt
  | Sltu
  | Xor
  | Srl
  | Sra
  | Or
  | And

type iinstr =
  | Jalr
  | Lb
  | Lh
  | Lw
  | Lbu
  | Lhu
  | Addi
  | Slti
  | Sltiu
  | Xori
  | Ori
  | Andi
  | Slli
  | Srli
  | Srai

type sinstr =
  | Sb
  | Sh
  | Sw

type binstr =
  | Beq
  | Bne
  | Blt
  | Bge
  | Bltu
  | Bgeu

type uinstr =
  | Lui
  | Auipc

type jinstr =
  | Jal

type instr =
  | Rtype of (rinstr * int * int * int) (* opcode, rd, rs1, rs2 *)
  | Itype of (iinstr * int * int * int) (* opcode, rd, rs1, imm *)
  | Stype of (sinstr * int * int * int) (* opcode, rs1, rs2, imm *)
  | Btype of (binstr * int * int * int) (* opcode, rs1, rs2, imm *)
  | Utype of (uinstr * int * int) (* opcode, rd, imm *)
  | Jtype of (jinstr * int * int) (* opcode, rd, imm *)
  | Illegal of Int32.t (* opcode *)

let sign_extend size n =
  let sign = 1 lsl size in
  if (n land sign) <> 0 then
    let extension = lnot (sign - 1) in
    n lor extension
  else
    n

let decode_op r = Int32.(logand r 0x7fl)
let decode_rd r = Int32.(to_int @@ shift_right (logand r 0xf80l) 7)
let decode_rs1 r = Int32.(to_int @@ shift_right (logand r 0xf8000l) 15)
let decode_rs2 r = Int32.(to_int @@ shift_right (logand r 0x1f00000l) 20)
let decode_funct3 r = Int32.(shift_right (logand r 0x7000l) 12)
let decode_funct7 r = Int32.(shift_right (logand r 0xfe000000l) 25)

let decode_r r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  match op, funct3, funct7 with
  | 0b0110011l, 0b000l, 0l -> Rtype (Add, rd, rs1, rs2)
  | 0b0110011l, 0b001l, 0l -> Rtype (Sll, rd, rs1, rs2)
  | 0b0110011l, 0b010l, 0l -> Rtype (Slt, rd, rs1, rs2)
  | 0b0110011l, 0b011l, 0l -> Rtype (Sltu, rd, rs1, rs2)
  | 0b0110011l, 0b100l, 0l -> Rtype (Xor, rd, rs1, rs2)
  | 0b0110011l, 0b101l, 0l -> Rtype (Srl, rd, rs1, rs2)
  | 0b0110011l, 0b110l, 0l -> Rtype (Or, rd, rs1, rs2)
  | 0b0110011l, 0b111l, 0l -> Rtype (And, rd, rs1, rs2)
  | 0b0110011l, 0b000l, 0b0100000l -> Rtype (Sub, rd, rs1, rs2)
  | 0b0110011l, 0b101l, 0b0100000l -> Rtype (Sra, rd, rs1, rs2)
  | _ -> Illegal r

let decode_i r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      shamt = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  let immediate = Int32.(to_int @@ shift_right (logand r 0xfff00000l) 20)
                |> sign_extend 11 in
  match op, funct3, funct7 with
  | 0b1100111l, 0l, _ -> Itype (Jalr, rd, rs1, immediate)
  | 0b0000011l, 0b000l, _ -> Itype (Lb, rd, rs1, immediate)
  | 0b0000011l, 0b001l, _ -> Itype (Lh, rd, rs1, immediate)
  | 0b0000011l, 0b010l, _ -> Itype (Lw, rd, rs1, immediate)
  | 0b0000011l, 0b100l, _ -> Itype (Lbu, rd, rs1, immediate)
  | 0b0000011l, 0b101l, _ -> Itype (Lhu, rd, rs1, immediate)
  | 0b0010011l, 0b000l, _ -> Itype (Addi, rd, rs1, immediate)
  | 0b0010011l, 0b010l, _ -> Itype (Slti, rd, rs1, immediate)
  | 0b0010011l, 0b011l, _ -> Itype (Sltiu, rd, rs1, immediate)
  | 0b0010011l, 0b100l, _ -> Itype (Xori, rd, rs1, immediate)
  | 0b0010011l, 0b110l, _ -> Itype (Ori, rd, rs1, immediate)
  | 0b0010011l, 0b111l, _ -> Itype (Andi, rd, rs1, immediate)
  | 0b0010011l, 0b001l, 0l -> Itype (Slli, rd, rs1, shamt)
  | 0b0010011l, 0b101l, 0l -> Itype (Srli, rd, rs1, shamt)
  | 0b0010011l, 0b101l, 0b0100000l -> Itype (Srai, rd, rs1, shamt)
  | _ -> Illegal r

let decode_s r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      lower_imm = decode_rd r in
  let immediate = Int32.(to_int @@ shift_right (logand r 0xfe000000l) 20)
                  |> (lor) lower_imm
                  |> sign_extend 11 in
  match funct3 with
  | 0b000l -> Stype (Sb, rs2, rs1, immediate)
  | 0b001l -> Stype (Sh, rs2, rs1, immediate)
  | 0b010l -> Stype (Sw, rs2, rs1, immediate)
  | _ -> Illegal r

let decode_b r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      immediate = Int32.(to_int
                           (shift_right (logand r 0x80000000l) 19
                            |> logor (shift_right (logand r 0x80l) 4)
                            |> logor (shift_right (logand r 0x7e000000l) 20)
                            |> logor (shift_right (logand r 0xf00l) 7)))
                  |> sign_extend 12 in
  match funct3 with
  | 0b000l -> Btype (Beq, rs1, rs2, immediate)
  | 0b001l -> Btype (Bne, rs1, rs2, immediate)
  | 0b100l -> Btype (Blt, rs1, rs2, immediate)
  | 0b101l -> Btype (Bge, rs1, rs2, immediate)
  | 0b110l -> Btype (Bltu, rs1, rs2, immediate)
  | 0b111l -> Btype (Bgeu, rs1, rs2, immediate)
  | _ -> Illegal r

let decode_u r =
  let op = decode_op r and
      rd = decode_rd r and
      immediate = Int32.(to_int @@ shift_right (logand r 0xffffff00l) 12) in
  match op with
  | 0b0110111l -> Utype (Lui, rd, immediate)
  | 0b0010111l -> Utype (Auipc, rd, immediate)
  | _ -> Illegal r

let decode_j r =
  let op = decode_op r and
      rd = decode_rd r in
  let immediate = Int32.(to_int
                           (logand r 0xff000l
                            |> logor (shift_right (logand r 0x100000l) 9)
                            |> logor (shift_right (logand r 0x7fe00000l) 20)
                            |> logor (shift_right (logand r 0x80000000l) 11)))
                  |> sign_extend 20 in
  if op = 0b1101111l then Jtype (Jal, rd, immediate)
  else Illegal r

let decode_instr r =
  match decode_op r with
  | 0b0110011l -> decode_r r
  | 0b1100111l | 0b0000011l | 0b0010011l -> decode_i r
  | 0b0100011l -> decode_s r
  | 0b1100011l -> decode_b r
  | 0b0110111l | 0b0010111l -> decode_u r
  | 0b1101111l -> decode_j r
  | _ -> Illegal r

let out_register = function
  | Rtype (_, rd, _, _)
  | Itype (_, rd, _, _)
  | Utype (_, rd, _)
  | Jtype (_, rd, _) -> Some rd
  | _ -> None

let print_rtype = function
  | Add -> "add"
  | Sub -> "sub"
  | Sll -> "sll"
  | Slt -> "slt"
  | Sltu -> "sltu"
  | Xor -> "xor"
  | Srl -> "srl"
  | Sra -> "sra"
  | Or -> "or"
  | And -> "and"

let print_itype = function
  | Jalr -> "jalr"
  | Lb -> "lb"
  | Lh -> "lh"
  | Lw -> "lw"
  | Lbu -> "lbu"
  | Lhu -> "lhu"
  | Addi -> "addi"
  | Slti -> "slti"
  | Sltiu -> "sltiu"
  | Xori -> "xori"
  | Ori -> "ori"
  | Andi -> "andi"
  | Slli -> "slli"
  | Srli -> "srli"
  | Srai -> "srai"

let print_stype = function
  | Sb -> "sb"
  | Sh -> "sh"
  | Sw -> "sw"

let print_btype = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Bge -> "bge"
  | Bltu -> "bltu"
  | Bgeu -> "bgeu"

let print_utype = function
  | Lui -> "lui"
  | Auipc -> "auipc"

let print_jtype = function
  | Jal -> "jal"

let register_to_abi = function
  | 0 -> "zero"
  | 1 -> "ra"
  | 2 -> "sp"
  | 3 -> "gp"
  | 4 -> "tp"
  | 5 -> "t0"
  | 6 -> "t1"
  | 7 -> "t7"
  | 8 -> "fp"
  | 9 -> "s1"
  | 10 -> "a0"
  | 11 -> "a1"
  | 12 -> "a2"
  | 13 -> "a3"
  | 14 -> "a4"
  | 15 -> "a5"
  | 16 -> "a6"
  | 17 -> "a7"
  | 18 -> "s2"
  | 19 -> "s3"
  | 20 -> "s4"
  | 21 -> "s5"
  | 22 -> "s6"
  | 23 -> "s7"
  | 24 -> "s8"
  | 25 -> "s9"
  | 26 -> "s10"
  | 27 -> "s11"
  | 28 -> "t3"
  | 29 -> "t4"
  | 30 -> "t5"
  | 31 -> "t6"
  | _ -> failwith "Invalid register number."

let fmt_register () = register_to_abi

let print_instr = function
  | Rtype (opcode, rd, rs1, rs2) ->
    Printf.sprintf "%s %a, %a, %a"
      (print_rtype opcode) fmt_register rd fmt_register rs1 fmt_register rs2
  | Itype (Lb | Lh | Lw | Lbu | Lhu as opcode, rd, rs1, imm) ->
    Printf.sprintf "%s %a, %d(%a)"
      (print_itype opcode) fmt_register rd imm fmt_register rs1
  | Itype (opcode, rd, rs1, imm) ->
    Printf.sprintf "%s %a, %a, %d"
      (print_itype opcode) fmt_register rd fmt_register rs1 imm
  | Stype (opcode, rs1, rs2, imm) ->
    Printf.sprintf "%s %a, %d(%a)"
      (print_stype opcode) fmt_register rs1 imm fmt_register rs2
  | Btype (opcode, rs1, rs2, imm) ->
    Printf.sprintf "%s %a, %a, 0x%X"
      (print_btype opcode) fmt_register rs1 fmt_register rs2 imm
  | Utype (opcode, rd, imm) ->
    Printf.sprintf "%s %a, 0x%X" (print_utype opcode) fmt_register rd imm
  | Jtype (opcode, rd, imm) ->
    Printf.sprintf "%s %a, 0x%X" (print_jtype opcode) fmt_register rd imm
  | Illegal instr -> Printf.sprintf "illegal instruction (0x%lX)" instr
