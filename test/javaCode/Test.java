public class Test {
    public Test() {}
    protected char x = (char)123;
    public static int test() {
	Test obj = new Test();
	char y = obj.x;
	return (int)y;
    }
}
