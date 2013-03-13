public class Test {
	public int foo;
	public Test() {}
	
	public static int test() {
		Test Test = new Test();
		Test.foo = 123;
		Test = null;
		return Test.foo;
	}
}

