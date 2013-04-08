package codegen

import ast._
import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.File
import main.CompilerError

class CodeGenError(str: String) extends CompilerError(str, "code generation")

object CodeGenerator {

  def iffalse(expr: Expression, label: X86Label)(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]): List[X86Instruction] = {
    expr.generateCode ::: (X86Mov(X86ebx, X86Number(0)) :: X86Cmp(X86eax, X86ebx) :: X86Je(label) :: Nil) //TODO:eax contains answer?
  }
  
  def makeLabel(p: Option[Name], c: ClassDefinition, s: String): String = {
    makeLabel(p, c.className, s)
  }
  
  def makeLabel(p: Option[Name], c: String, s: String): String = {
    (p match {
      case Some(x) => x + "."
      case None => ""
    }) + c + "." + s
  }
  
  def makeFieldLabel(p: Option[Name], c: ClassDefinition, f: FieldDeclaration) = {
    makeLabel(p, c, f.fieldName)
  }

  def makeMethodLabel(p: Option[Name], c: ClassDefinition, m: MethodDeclaration) = {
    makeLabel(p, c, m.methodName + "$" +
      m.parameters.map(_.paramType.typeName.replaceAllLiterally("[]", "$")).mkString("_"))
  }

  def makeConstructorLabel(p: Option[Name], c: ClassDefinition, cons: ConstructorDeclaration) = {
    makeLabel(p, c, "$constructor$" +
      cons.parameters.map(_.paramType.typeName.replaceAllLiterally("[]", "$")).mkString("_"))
  }
  
  def getVtable(pkg: Option[Name], cd: ClassDefinition, cus: List[CompilationUnit]): List[MethodDeclaration] = {
    getMethods(pkg, cd, Nil, cus).map(_._3)
  }
  
  def getFields(cd: ClassDefinition, cus: List[CompilationUnit]): List[FieldDeclaration] = {
    cd.fields.filterNot(_.modifiers.contains(Modifier.staticModifier)) :::
    (cd.parent match {
      case None => Nil
      case Some(p) =>
        getFields(p.asInstanceOf[RefTypeLinked].getTypeDef(cus).asInstanceOf[ClassDefinition], cus)
    })   
  }

  private def getMethods(pkg: Option[Name], cd: ClassDefinition, parentMethods: List[(Option[Name], ClassDefinition, MethodDeclaration)], cus: List[CompilationUnit]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {

    def methodsMatch(m1: MethodDeclaration, m2: MethodDeclaration): Boolean = {
      m1.methodName == m2.methodName && m1.parameters == m2.parameters
    }

    def mergeMethods(ms: List[MethodDeclaration], ts: List[(Option[Name], ClassDefinition, MethodDeclaration)]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {
      ms match {
        case Nil => ts
        case m :: mss =>
          mergeMethods(mss, ts.find(t => methodsMatch(m, t._3)) match {
            case None => (pkg, cd, m) :: ts
            case Some(x) => x :: ts.filter(t => methodsMatch(m, t._3))
          })
      }
    }

    val replaced = mergeMethods(cd.methods.filterNot(_.modifiers.contains(Modifier.staticModifier)), parentMethods)
    cd.parent match {
      case None => replaced
      case Some(p) =>
        val linked = p.asInstanceOf[RefTypeLinked]
        getMethods(linked.pkgName, linked.getTypeDef(cus).asInstanceOf[ClassDefinition], replaced, cus)
    }
  }

  /**
   * generate files in the output/ directory
   */
  def makeAssembly(cus: List[CompilationUnit]): Unit = {
    // DUMMY CODE
    /*
	val writer = new BufferedWriter(new FileWriter(new File("output/simple.s")))
	writer.write("""
	
	global _start
	_start:
	
	mov eax, 1
	mov ebx, 123
	int 0x80
	
	          
	""")
	writer.close
	*/

    val firstCu = cus.head
    val mainFuncLabel = makeLabel(firstCu.packageName, firstCu.typeName, "test$")
    val writer = new BufferedWriter(new FileWriter(new File("output/$main.s")))
    writer.write(
s"""
extern $mainFuncLabel
extern __debexit
        
global _start:
_start:
  call $mainFuncLabel
  jmp __debexit
""")
	writer.close
	
    val include = new BufferedWriter(new FileWriter(new File("output/asm.inc")))
    
    def generate(cu: CompilationUnit, cd: ClassDefinition)(implicit cus:List[CompilationUnit]): String = { //we just need the CU for the full name
      
      val methods = getMethods(cu.packageName, cd, Nil, cus)
      
      include.write(cd.methods.map(m => "extern " + makeMethodLabel(cu.packageName, cd, m)).mkString("\n") + "\n")

      ///////////////// header ///////////////////////
      val header =
        "extern __malloc\n" +
        "extern __exception\n" +
        "extern __debexit\n" +
        /*methods.filterNot(t => t._1 == cu.packageName && t._2 == cd)
               .map(t => s"extern ${makeMethodLabel(t._1, t._2, t._3)}")
               .mkString("\n") +*/
        "%include \"asm.inc\"\n"
        "\n\n"
      ///////////////// end of header/////////////////
      
      ///////////////// data segment /////////////////
      val data =
        "section .data\n\n" +
        "; VTABLE\n" +
        "class:\n" + 
        "  dd 0 ; TODO: pointer to SIT\n  " +
        methods.map(t => s"dd ${makeMethodLabel(t._1, t._2, t._3)}")
               .mkString("\n  ") +
        "\n\n"
      ///////////////// end of data segment /////////

      ///////////////// bss segment /////////////////
      val staticFields = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier))
      val bss =
        "section .bss\n\n" +
        "; static fields\n" +
        staticFields.map(f => s"${makeFieldLabel(cu.packageName, cd, f)}: resb 4").mkString("\n") + "\n\n"
      ///////////////// end of bss segment //////////
  
      ///////////////// text segment /////////////////
      val fields = getFields(cd, cus)
      val text = (
        "section .text\n\n" +
        // === static initialization ===
        "global " + makeLabel(cu.packageName, cd, "$static_init") + "\n" +
        makeLabel(cu.packageName, cd, "$static_init") + ":\n" +
        staticFields.map(f =>
          "  ; " + f.fieldName + "\n" +
          (f.initializer match {
            case Some(expr) => expr.generateCode(List(0), Nil, Nil, cus).mkString("\n") +
                               s"\n  mov [${makeFieldLabel(cu.packageName, cd, f)}], eax"
            case None => s"  mov [${makeFieldLabel(cu.packageName, cd, f)}], dword 0"
          })).mkString("\n") +
        "\n  ret\n\n" +	
        // === instance allocation ===
        "global " + makeLabel(cu.packageName, cd, "$alloc") + "\n" +
        makeLabel(cu.packageName, cd, "$alloc") + ":\n" +
        "  mov eax, " + ((fields.length + 1) * 4) + "\n" +
        "  call __malloc\n" +
        "  mov ebx, eax\n" +
        "  push ebx\n" +
        "  mov [ebx], dword class ; set pointer to class\n" +
        fields.zipWithIndex.map(z =>
          "  ; initializing " + z._1.fieldName + "\n" +
          (z._1.initializer match {
            case Some(expr) => "  mov ebx, [esp]\n" +
                               expr.generateCode(List(0), Nil, Nil, cus).mkString("\n") +
                               s"\n  mov [ebx + ${(z._2 + 1)*4}], eax\n"
            case None => s"  mov [eax + ${(z._2 + 1)*4}], dword 0\n"
          })).mkString("\n") +
        "  pop eax\n" +
        "  ret\n\n" +
        // === constructors ===
        cd.constructors.map(c => {
          val lbl = makeConstructorLabel(cu.packageName, cd, c)
          "global " + lbl + "\n" +
          lbl + ":\n" +
          c.generateCode.mkString("\n")
        }).mkString("\n\n") +
        "\n\n" +
        // === methods ===
        cd.methods.map(m => {
          val lbl = makeMethodLabel(cu.packageName, cd, m)
          "global " + lbl + "\n" +
          lbl + ":\n" +
          m.generateCode.mkString("\n")
        }).mkString("\n\n")
      )
      ///////////////// end of text segment //////////
       
      "; === " + cd.className + "===\n" + header + data + bss + text
    }
    
    cus
    //leave the java lib files out for the moment! -> makes testing easier
    //.filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
    .collect { case cu @ CompilationUnit(optName, _, Some(d: ClassDefinition), name) =>
      val writer = new BufferedWriter(new FileWriter(new File("output/"+cu.typeName+".s")))
        //println("class: " + cu.typeName)
      val code = generate(cu, d)(cus)
      writer.write(code)
      writer.close
    }
    
    include.close
  }
}
