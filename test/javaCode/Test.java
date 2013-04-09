public class Test {
	public Test() {}

	public static int func() {
		int x = 5;
		return x + 2;
	}

	public static int test() {
		int x = Test.func();
		return x;
	}
}
