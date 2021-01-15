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
      lower_imm = decode_rd r and
      higher_imm = decode_funct7 r in
  let immediate = sign_extend 11 ((lower_imm lsr 7) lor (higher_imm lsr 20)) in
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

let print_instr = function
  | Rtype (opcode, rd, rs1, rs2) ->
    Printf.sprintf "%s x%d, x%d, x%d" (print_rtype opcode) rd rs1 rs2
  | Itype (Lb | Lh | Lw | Lbu | Lhu as opcode, rd, rs1, imm) ->
    Printf.sprintf "%s x%d, %d(x%d)" (print_itype opcode) rd imm rs1
  | Itype (opcode, rd, rs1, imm) ->
    Printf.sprintf "%s x%d, x%d, %d" (print_itype opcode) rd rs1 imm
  | Stype (opcode, rs1, rs2, imm) ->
    Printf.sprintf "%s x%d, %d(x%d)" (print_stype opcode) rs1 imm rs2
  | Btype (opcode, rs1, rs2, imm) ->
    Printf.sprintf "%s x%d, x%d, %d" (print_btype opcode) rs1 rs2 imm
  | Utype (opcode, rd, imm) ->
    Printf.sprintf "%s x%d, %d" (print_utype opcode) rd imm
  | Jtype (opcode, rd, imm) ->
    Printf.sprintf "%s x%d, %d" (print_jtype opcode) rd imm
  | Illegal instr -> Printf.sprintf "illegal instruction (0x%X)" instr
