package codegen

// a valid source in an instruction
trait X86Src {
}
// a valid destination in a instruction
trait X86Dest extends X86Src
// label
case class X86Label(name:String) extends X86Src with X86Instruction { //TODO: can it also be a X86Dest
  override def toString = name+":"
}
// an instruction
trait X86Instruction {
}
// data
trait X86Data {
}

//comment
case class X86Comment(s:String) extends X86Instruction {
  override def toString = ";"+s
} 
// program
class X86Program(code: List[(Option[X86Label],X86Instruction)]) // TODO: add data definitions; multiple files?

// Addressing modes
// e.g. mov ax, bx:
class X86Reg(s:String) extends X86Dest {
  override def toString = s
}
object X86eax extends X86Reg("eax")	//return value	calleR saved
object X86ebx extends X86Reg("ebx")	//				callee saved
object X86ecx extends X86Reg("ecx")	//				calleR saved
object X86edx extends X86Reg("edx")	//				calleR saved
object X86esi extends X86Reg("esi") //source		callee saved
object X86edi extends X86Reg("edi") //destination	callee saved
object X86esp extends X86Reg("esp") //stack pointer	callee saved	->push and pop affect this
object X86ebp extends X86Reg("ebp") //frame pointer	callee saved
// e.g. mov ax, 1:
trait X86Immediate extends X86Src
case class X86Number(i: Int) extends X86Immediate {
  override def toString = ""+i
}
case class X86Boolean(b: Boolean) extends X86Immediate {
  override def toString = b match { //TODO should be an int over 32 bits...
    case true => ""+1
    case false => ""+0
  }
}
// e.g. mov ax, [102h]:
case class X86DirectMemoryAccess(imm: X86Immediate) extends X86Dest {
  override def toString = "["+imm+"]"
}
// e.g. mov ax,[di]:
case class X86RegMemoryAccess(reg: X86Reg) extends X86Dest {
  override def toString = "["+reg+"]"
}
// e.g. mov al,[byte_tbl+2]:
case class X86RegOffsetMemoryAccess(reg: X86Reg, imm: X86Immediate) extends X86Dest {
  override def toString = "["+reg+" "+imm+"]"
}
// e.g. mov ax,[bx + di]:
case class X86BaseIndexMemoryAccess(reg1: X86Reg, reg2: X86Reg) extends X86Dest {
  override def toString = "["+reg1+" + "+reg2+"]"
}
// e.g. mov ax,[bx + di + 10]:
case class X86BaseIndexDisplacementMemoryAccess(reg1: X86Reg, reg2: X86Reg, imm: X86Immediate) extends X86Dest

//data
// e.g. db value
case class X86DataByte(b:Byte) extends X86Data //8 bits
// e.g. dw value
case class X86DataWord(c:Char) extends X86Data //16 bits
// e.g. label dd
case class X86DataDoubleWordUninitialized(label:X86Label) extends X86Data {//32 bits true -> null terminated
  override def toString = "\t"+label+" dd"
}
// e.g. dd value
case class X86DataDoubleWord(s:String) extends X86Data //32 bits true -> null terminated
// db "abcd", 0
case class X86DataNullTerminatedWord(s:String, i:Integer) extends X86Data //32 bits null terminated

// Instructions

case class X86Bcc   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"bcc $dest, $src"
}
case class X86Neg   (dest: X86Dest)              extends X86Instruction {
  override def toString = s"neg $dest"
}
case class X86Mov   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "mov "+dest+", "+src
}
case class X86Add   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "add "+dest+", "+src
}
case class X86Sub   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "sub "+dest+", "+src
}
case class X86Mul   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "mul "+dest+", "+src
}
case class X86Imul  (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "imul "+dest+", "+src
}
case class X86Div   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "div "+dest+", "+src
}
case class X86Idiv  (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = "idiv "+dest+", "+src
}
case class X86Jmp   (lbl: X86Label)              extends X86Instruction {
  override def toString = "jmp "+lbl
}
case class X86Cmp   (a: X86Reg    , b: X86Reg)   extends X86Instruction {
  override def toString = "cmp "+a+", "+b //TODO: where will the result be stored? eax?
}
case class X86Je    (lbl: X86Label)              extends X86Instruction {
  override def toString = "je "+lbl
}
case class X86Jne   (lbl: X86Label)              extends X86Instruction {
  override def toString = "jne "+lbl
}
case class X86Jg    (lbl: X86Label)              extends X86Instruction {
  override def toString = "jg "+lbl
}
case class X86Jl    (lbl: X86Label)              extends X86Instruction {
  override def toString = "jl "+lbl
}
case class X86Jge   (lbl: X86Label)              extends X86Instruction {
  override def toString = "jge "+lbl
}
case class X86Jle   (lbl: X86Label)              extends X86Instruction {
  override def toString = "jle "+lbl
}
case class X86Call  (lbl: X86Label)              extends X86Instruction {
  override def toString = "call "+lbl
}
case object X86Ret                               extends X86Instruction {
  override def toString = "ret"
}
case class X86Push  (reg: X86Reg)                extends X86Instruction {
  override def toString = "push "+reg
}
case class X86Pop   (reg: X86Reg)                extends X86Instruction {
  override def toString = "pop "+reg
}
case class X86Int   (imm: X86Immediate)                extends X86Instruction {
  override def toString = "int "+imm //system call -> can only be 2 bytes long
}
