public class Test {
	public int x = 1;
	
	public Test() {
		//x = 2;
	}
	
	public int method() {
		return 10;//x;
	}

	public static int test() {
		Test t = new Test();
		return t.method();
	}
}
