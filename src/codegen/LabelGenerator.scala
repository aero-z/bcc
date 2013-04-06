package codegen

object LabelGenerator {
  private var counter:Int = 10000;// better/worse?
  //private var state:State = State()
  def generate:X86Label = {
    counter = counter+1
    X86Label(""+counter);
  }
}
