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
  | Illegal of int (* opcode *)

let sign_extend size n =
  let sign = 1 lsl size in
  if (n land sign) <> 0 then
    let extension = lnot (sign - 1) in
    n lor extension
  else
    n

let decode_op r = r land 0x7f
let decode_rd r = (r land 0xf80) lsr 7
let decode_rs1 r = (r land 0xf8000) lsr 15
let decode_rs2 r = (r land 0x1f00000) lsr 20
let decode_funct3 r = (r land 0x7000) lsr 12
let decode_funct7 r = (r land 0xfe000000) lsr 25

let decode_r r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  match op, funct3, funct7 with
  | 0b0110011, 0b000, 0 -> Rtype (Add, rd, rs1, rs2)
  | 0b0110011, 0b001, 0 -> Rtype (Sll, rd, rs1, rs2)
  | 0b0110011, 0b010, 0 -> Rtype (Slt, rd, rs1, rs2)
  | 0b0110011, 0b011, 0 -> Rtype (Sltu, rd, rs1, rs2)
  | 0b0110011, 0b100, 0 -> Rtype (Xor, rd, rs1, rs2)
  | 0b0110011, 0b101, 0 -> Rtype (Srl, rd, rs1, rs2)
  | 0b0110011, 0b110, 0 -> Rtype (Or, rd, rs1, rs2)
  | 0b0110011, 0b111, 0 -> Rtype (And, rd, rs1, rs2)
  | 0b0110011, 0b000, 0b0100000 -> Rtype (Sub, rd, rs1, rs2)
  | 0b0110011, 0b101, 0b0100000 -> Rtype (Sra, rd, rs1, rs2)
  | _ -> Illegal r

let decode_i r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      shamt = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  let immediate = sign_extend 11 ((r land 0xfff00000) lsr 20) in
  match op, funct3, funct7 with
  | 0b1100111, 0, _ -> Itype (Jalr, rd, rs1, immediate)
  | 0b0000011, 0b000, _ -> Itype (Lb, rd, rs1, immediate)
  | 0b0000011, 0b001, _ -> Itype (Lh, rd, rs1, immediate)
  | 0b0000011, 0b010, _ -> Itype (Lw, rd, rs1, immediate)
  | 0b0000011, 0b100, _ -> Itype (Lbu, rd, rs1, immediate)
  | 0b0000011, 0b101, _ -> Itype (Lhu, rd, rs1, immediate)
  | 0b0010011, 0b000, _ -> Itype (Addi, rd, rs1, immediate)
  | 0b0010011, 0b010, _ -> Itype (Slti, rd, rs1, immediate)
  | 0b0010011, 0b011, _ -> Itype (Sltiu, rd, rs1, immediate)
  | 0b0010011, 0b100, _ -> Itype (Xori, rd, rs1, immediate)
  | 0b0010011, 0b110, _ -> Itype (Ori, rd, rs1, immediate)
  | 0b0010011, 0b111, _ -> Itype (Andi, rd, rs1, immediate)
  | 0b0010011, 0b001, 0 -> Itype (Slli, rd, rs1, shamt)
  | 0b0010011, 0b101, 0 -> Itype (Srli, rd, rs1, shamt)
  | 0b0010011, 0b101, 0b0100000 -> Itype (Srai, rd, rs1, shamt)
  | _ -> Illegal r

let decode_s r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      lower_imm = decode_rd r in
  let immediate = lower_imm lor ((r land 0xfe000000) lsr 20)
                  |> sign_extend 11 in
  match funct3 with
  | 0b000 -> Stype (Sb, rs2, rs1, immediate)
  | 0b001 -> Stype (Sh, rs2, rs1, immediate)
  | 0b010 -> Stype (Sw, rs2, rs1, immediate)
  | _ -> Illegal r

let decode_b r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      immediate = ((r land 0x80000000) lsr 19) lor ((r land 0x80) lsl 4)
                  lor ((r land 0x7e000000) lsr 20) lor ((r land 0xf00) lsr 7)
                  |> sign_extend 12 in
  match funct3 with
  | 0b000 -> Btype (Beq, rs1, rs2, immediate)
  | 0b001 -> Btype (Bne, rs1, rs2, immediate)
  | 0b100 -> Btype (Blt, rs1, rs2, immediate)
  | 0b101 -> Btype (Bge, rs1, rs2, immediate)
  | 0b110 -> Btype (Bltu, rs1, rs2, immediate)
  | 0b111 -> Btype (Bgeu, rs1, rs2, immediate)
  | _ -> Illegal r

let decode_u r =
  let op = decode_op r and
      rd = decode_rd r and
      immediate = (r land 0xffffff00) lsr 12 in
  match op with
  | 0b0110111 -> Utype (Lui, rd, immediate)
  | 0b0010111 -> Utype (Auipc, rd, immediate)
  | _ -> Illegal r

let decode_j r =
  let op = decode_op r and
      rd = decode_rd r in
  let immediate = (r land 0xff000) lor ((r land 0x100000) lsr 9)
                  lor ((r land 0x7fe00000) lsr 20)
                  lor ((r land 0x80000000) lsr 11)
                  |> sign_extend 20 in
  if op = 0b1101111 then Jtype (Jal, rd, immediate)
  else Illegal r

let decode_instr r =
  match decode_op r with
  | 0b0110011 -> decode_r r
  | 0b1100111 | 0b0000011 | 0b0010011 -> decode_i r
  | 0b0100011 -> decode_s r
  | 0b1100011 -> decode_b r
  | 0b0110111 | 0b0010111 -> decode_u r
  | 0b1101111 -> decode_j r
  | _ -> Illegal r

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
  | Illegal instr -> Printf.sprintf "illegal instruction (0x%X)" instr
