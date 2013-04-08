package codegen

object LabelGenerator {
  private var counter:Int = 0
  
  def generate(postfix: String = ""): X86Label = {
    val post = (if (postfix.isEmpty) ""
               else ("_" + postfix))
    counter = counter+1
    X86Label("_"+counter+post);
  }
}
