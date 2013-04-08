package codegen

// a valid source in an instruction
trait X86Src {
}
// a valid destination in a instruction
class X86Dest extends X86Function with X86Src {
  val name = toString
}
// label
case class X86Label(override val name: String) extends X86Function with X86Instruction { //TODO: can it also be a X86Dest
  override def toString = name + ":"
}

object X86Exception extends X86Label("__exception")
// an instruction
trait X86Instruction {
}
// data
trait X86Data {
}

abstract class X86Function {
  val name: String
}

//comment
case class X86Comment(s:String) extends X86Instruction {
  override def toString = ";"+s
}
case class X86Extern(s: String) extends X86Instruction {
  override def toString = "  extern "+s
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
  override def toString = "dword "+i
}
// e.g. mov ax, [102h]:
case class X86DirectMemoryAccess(imm: Int) extends X86Dest {
  override def toString = s"[$imm]"
}
// e.g. mov ax,[di]:
case class X86RegMemoryAccess(reg: X86Reg) extends X86Dest {
  override def toString = s"[$reg]"
}
// e.g. mov ax,[di]:
case class X86LblMemoryAccess(lbl: X86Label) extends X86Dest {
  override def toString = s"[${lbl.name}]"
}
// e.g. mov al,[byte_tbl+2]:
case class X86RegOffsetMemoryAccess(reg: X86Reg, imm: Int) extends X86Dest {
  override def toString = s"[$reg + $imm]"
}
// e.g. mov ax,[bx + di]:
case class X86BaseIndexMemoryAccess(reg1: X86Reg, reg2: X86Reg) extends X86Dest {
  override def toString = s"[$reg1 + $reg2]"
}
// e.g. mov ax,[bx + di + 10]:
case class X86BaseIndexDisplacementMemoryAccess(reg1: X86Reg, reg2: X86Reg, imm: Int) extends X86Dest {
  override def toString = s"[$reg1 + $reg2 + $imm]"
}

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

case class X86Sbb   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  sbb $dest, $src"
}
case class X86Neg   (dest: X86Dest)              extends X86Instruction {
  override def toString = s"  neg $dest"
}
case class X86Mov   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  mov $dest, $src"
}
case class X86Add   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  add $dest, $src"
}
case class X86Sub   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  sub $dest, $src"
}
case class X86Mul   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  mul $dest, $src"
}
case class X86Imul  (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  imul $dest, $src"
}
case class X86Div   (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  div $dest, $src"
}
case class X86Idiv  (dest: X86Dest, src: X86Src) extends X86Instruction {
  override def toString = s"  idiv $dest, $src"
}
case class X86Shl   (dest: X86Dest, src: Int) extends X86Instruction {
  override def toString = s"  shl $dest, $src"
}
case class X86Jmp   (lbl: X86Label)              extends X86Instruction {
  override def toString = "  jmp "+lbl.name
}
case class X86Bxor   (dest: X86Dest    , src: X86Src)   extends X86Instruction {
  override def toString = s"  xor $dest, $src"
}
case class X86Band   (dest: X86Dest    , src: X86Src)   extends X86Instruction {
  override def toString = s"  and $dest, $src"
}
case class X86Bor   (dest: X86Dest    , src: X86Src)   extends X86Instruction {
  override def toString = s"  or $dest, $src"
}
case class X86Cmp   (dest: X86Dest    , src: X86Src)   extends X86Instruction {
  override def toString = s"  cmp $dest, $src" //will set some flags -> not eax but other stuff?
}
case class X86Je    (lbl: X86Function)              extends X86Instruction {
  override def toString = "  je "+lbl.name
}
case class X86Jne   (lbl: X86Function)              extends X86Instruction {
  override def toString = "  jne "+lbl.name
}
case class X86Jg    (lbl: X86Function)              extends X86Instruction {
  override def toString = "  jg "+lbl.name
}
case class X86Jl    (lbl: X86Function)              extends X86Instruction {
  override def toString = "  jl "+lbl.name
}
case class X86Jge   (lbl: X86Function)              extends X86Instruction {
  override def toString = "  jge "+lbl.name
}
case class X86Jle   (lbl: X86Function)              extends X86Instruction {
  override def toString = "  jle "+lbl.name
}
case class X86Call  (lbl: X86Function)              extends X86Instruction {
  override def toString = "  call "+lbl.name
}
case object X86Ret                               extends X86Instruction {
  override def toString = "  ret"
}
case class X86Push  (reg: X86Reg)                extends X86Instruction {
  override def toString = s"  push $reg"
}
case class X86Pop   (reg: X86Reg)                extends X86Instruction {
  override def toString = s"  pop $reg"
}
case class X86Int   (imm: X86Immediate)                extends X86Instruction {
  override def toString = s"  int $imm" //system call -> can only be 2 bytes long
}


object X86Gen{
  //Check if the source is null, in this case throw an error
  def nullCheck(dest: X86Dest) = List(X86Cmp(dest , X86Number(0)), X86Je(X86Exception))
  def notImpl = List(X86Jmp(X86Exception))
}
