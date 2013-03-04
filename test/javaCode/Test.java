// PARSER_WEEDER
public class Test {
    public Test() {}
    
    public static int test() {
	int a = -2147483648;
	if (a-1 > a) 
	    return 123;
	return 42;
    }
}
