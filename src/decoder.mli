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

val decode_instr : int -> instr
