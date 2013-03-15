public class Test {
	public Test() {
	}

	public static int test() {
		int x = 117;
		String s = "foo" + x + true + null + 'z' + "" + " ";
		System.out.println(s);
		if (s.equals((Object) "foo117truenullz "))
			return 123;
		return 0;

	}
}
