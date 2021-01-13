type rinstr =
  | Add
  | Sub
  | Sll
  | Slt
  | Sltu
  | Xor
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

let decode_op r = r & 0x7f
let decode_rd r = (r & 0x7c0) lsr 7
let decode_rs1 r = (r & 0xf8000) lsr 15 
let decode_rs2 r = (r & 0x1f00000) lsr 20
let decode_funct3 r = (r & 0x700) lsr 12
let decode_funct7 r = (r & 0xfe000000) lsr 25

let decode_r r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  match op, funct3, funct7 with
  | 0b0110011, 0, 0b000 -> Rtype (Add, rd, rs1, rs2)
  | 0b0110011, 0, 0b001 -> Rtype (Sll, rd, rs1, rs2)
  | 0b0110011, 0, 0b010 -> Rtype (Slt, rd, rs1, rs2)
  | 0b0110011, 0, 0b011 -> Rtype (Sltu, rd, rs1, rs2)
  | 0b0110011, 0, 0b100 -> Rtype (Xor, rd, rs1, rs2)
  | 0b0110011, 0, 0b101 -> Rtype (Slr, rd, rs1, rs2)
  | 0b0110011, 0, 0b110 -> Rtype (Or, rd, rs1, rs2)
  | 0b0110011, 0, 0b111 -> Rtype (And, rd, rs1, rs2)
  | 0b0110011, 0b0100000, 0b000 -> Rtype (Sub, rd, rs1, rs2)
  | 0b0110011, 0b0100000, 0b101 -> Rtype (Sra, rd, rs1, rs2)
  | _ -> Illegal r

let decode_i r =
  let op = decode_op r and
      rd = decode_rd r and
      rs1 = decode_rs1 r and
      shamt = decode_rs2 r and
      funct3 = decode_funct3 r and
      funct7 = decode_funct7 r in
  match op, funct3, funct7 with
  | 0b1100111, 0, _ -> Itype (Jalr, rd, rs1, 0)
  | 0b0000011, 0b000, _ -> Itype (Lb, rd, rs1, 0)
  | 0b0000011, 0b001, _ -> Itype (Lh, rd, rs1, 0)
  | 0b0000011, 0b010, _ -> Itype (Lw, rd, rs1, 0)
  | 0b0000011, 0b100, _ -> Itype (Lbu, rd, rs1, 0)
  | 0b0000011, 0b101, _ -> Itype (Lhu, rd, rs1, 0)
  | 0b0010011, 0b000, _ -> Itype (Addi, rd, rs1, 0)
  | 0b0010011, 0b010, _ -> Itype (Slti, rd, rs1, 0)
  | 0b0010011, 0b011, _ -> Itype (Sltiu, rd, rs1, 0)
  | 0b0010011, 0b100, _ -> Itype (Xori, rd, rs1, 0)
  | 0b0010011, 0b110, _ -> Itype (Ori, rd, rs1, 0)
  | 0b0010011, 0b111, _ -> Itype (Andi, rd, rs1, 0)
  | 0b0010011, 0b001, 0 -> Itype (Slli, rd, rs1, shamt)
  | 0b0010011, 0b101, 0 -> Itype (Srli, rd, rs1, shamt)
  | 0b0010011, 0b101, 0b0100000 -> Itype (Srai, rd, rs1, shamt)
  | _ -> Illegal r

let decode_s r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r in
  match funct3 with
  | 0b000 -> Stype (Sb, rs1, rs2, 0)
  | 0b001 -> Stype (Sh, rs1, rs2, 0)
  | 0b010 -> Stype (Sw, rs1, rs2, 0)
  | _ -> Illegal r

let decode_b r =
  let rs1 = decode_rs1 r and
      rs2 = decode_rs2 r and
      funct3 = decode_funct3 r in
  match funct3 with
  | 0b000 -> Btype (Beq, rs1, rs2, 0)
  | 0b001 -> Btype (Bne, rs1, rs2, 0)
  | 0b100 -> Btype (Blt, rs1, rs2, 0)
  | 0b101 -> Btype (Bge, rs1, rs2, 0)
  | 0b110 -> Btype (Bltu, rs1, rs2, 0)
  | 0b111 -> Btype (Bgeu, rs1, rs2, 0)
  | _ -> Illegal r

let decode_u r =
  let op = decode_op r and
      rd = decode_rd r in
  match op with
  | 0b0110111 -> Utype (Lui, rd, 0)
  | 0b0010111 -> Utype (Auipc, rd, 0)
  | _ -> Illegal r

let decode_j r =
  let op = decode_op r and
      rd = decode_rd r in
  if op = 0b1101111 then Rtype (Jal, rd, 0)
  else Illegal r

let decode_instr r =
  match decode_op r with
  | 0b0110011 -> decode_r r
  | 0b1100111 | 0b0000011 | 0b0010011 -> decode_i r
  | 0b0100011 -> decode_s r
  | 0b1100011 -> decode_b r
  | 0b0110111 | 0b0010111 -> decode_u r
  | 0b1101111 -> decode_j
  | _ -> Illegal r
