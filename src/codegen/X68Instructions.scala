package codegen

// a valid source in an instruction
trait X86Src {
}
// a valid destination in a instruction
trait X86Dest extends X86Src
// label
case class X86Label(name:String) extends X86Dest { //TODO: is this correct?
  override def toString = name
}
// an instruction
trait X86Instruction {
}
// data
trait X86Data {
}
// program
class X86Program(code: List[(Option[X86Label],X86Instruction)]) // TODO: add data definitions; multiple files?

// Addressing modes
// e.g. mov ax, bx:
trait X86Reg extends X86Dest
object X86eax extends X86Reg
// e.g. mov ax, 1:
trait X86Immediate extends X86Src
// e.g. mov ax, [102h]:
case class X86DirectMemoryAccess(imm: X86Immediate) extends X86Dest
// e.g. mov ax,[di]:
case class X86RegMemoryAccess(reg: X86Reg) extends X86Dest
// e.g. mov al,[byte_tbl+2]:
case class X86RegOffsetMemoryAccess(reg: X86Reg, imm: X86Immediate) extends X86Dest
// e.g. mov ax,[bx + di]:
case class X86BaseIndexMemoryAccess(reg1: X86Reg, reg2: X86Reg) extends X86Dest
// e.g. mov ax,[bx + di + 10]:
case class X86BaseIndexDisplacementMemoryAccess(reg1: X86Reg, reg2: X86Reg, imm: X86Immediate) extends X86Dest

//data
// e.g. db value
case class X86DataByte(b:Byte) extends X86Data //8 bits
// e.g. dw value
case class X86DataWord(c:Char) extends X86Data //16 bits
// e.g. label dd
case class X86DataDoubleWordUninitialized(label:String) extends X86Data {//32 bits true -> null terminated
  override def toString = "\t"+label+" dd"
}
// e.g. dd value
case class X86DataDoubleWord(s:String) extends X86Data //32 bits true -> null terminated
// db "abcd", 0
case class X86DataNullTerminatedWord(s:String, i:Integer) extends X86Data //32 bits null terminated

// Instructions
case class X86Mov   (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Add   (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Sub   (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Mul   (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Imul  (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Div   (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Idiv  (dest: X86Dest, src: X86Src) extends X86Instruction
case class X86Jmp   (lbl: X86Label)              extends X86Instruction
case class X86Cmp   (lbl: X86Label)              extends X86Instruction
case class X86Je    (lbl: X86Label)              extends X86Instruction
case class X86Jne   (lbl: X86Label)              extends X86Instruction
case class X86Jg    (lbl: X86Label)              extends X86Instruction
case class X86Jl    (lbl: X86Label)              extends X86Instruction
case class X86Jge   (lbl: X86Label)              extends X86Instruction
case class X86Jle   (lbl: X86Label)              extends X86Instruction
case class X86Call  (lbl: X86Label)              extends X86Instruction
case object X86Ret                               extends X86Instruction
case class X86Push  (reg: X86Reg)                extends X86Instruction
case class X86Pop   (reg: X86Reg)                extends X86Instruction
case class X86Int   (reg: X86Reg)                extends X86Instruction
