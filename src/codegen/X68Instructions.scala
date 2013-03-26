package codegen

// a valid source in an instruction
trait X68Src
// a valid destination in a instruction
trait X68Dest extends X68Src
// label
class X68Label
// an instruction
trait X68Instruction
// program
class X68Program(code: List[(Option[X68Label],X68Instruction)]) // TODO: add data definitions; multiple files?

// Addressing modes
// e.g. mov ax, bx:
class X68Reg extends X68Dest
// e.g. mov ax, 1:
class X68Immediate extends X68Src
// e.g. mov ax, [102h]:
class X68DirectMemoryAccess(imm: X68Immediate) extends X68Dest
// e.g. mov ax,[di]:
class X68RegMemoryAccess(reg: X68Reg) extends X68Dest
// e.g. mov al,[byte_tbl+2]:
class X68RegOffsetMemoryAccess(reg: X68Reg, imm: X68Immediate) extends X68Dest
// e.g. mov ax,[bx + di]:
class X68BaseIndexMemoryAccess(reg1: X68Reg, reg2: X68Reg) extends X68Dest
// e.g. mov ax,[bx + di + 10]:
class X68BaseIndexDisplacementMemoryAccess(reg1: X68Reg, reg2: X68Reg, imm: X68Immediate) extends X68Dest

// Instructions
case class X68Mov   (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Add   (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Sub   (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Mul   (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Imul  (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Div   (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Idiv  (dest: X68Dest, src: X68Src) extends X68Instruction
case class X68Jmp   (lbl: X68Label)              extends X68Instruction
case class X68Cmp   (lbl: X68Label)              extends X68Instruction
case class X68Je    (lbl: X68Label)              extends X68Instruction
case class X68Jne   (lbl: X68Label)              extends X68Instruction
case class X68Jg    (lbl: X68Label)              extends X68Instruction
case class X68Jl    (lbl: X68Label)              extends X68Instruction
case class X68Jge   (lbl: X68Label)              extends X68Instruction
case class X68Jle   (lbl: X68Label)              extends X68Instruction
