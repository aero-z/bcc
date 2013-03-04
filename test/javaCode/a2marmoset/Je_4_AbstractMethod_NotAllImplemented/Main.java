// JOOS1:HIERARCHY,CLASS_MUST_BE_ABSTRACT
// JOOS2:HIERARCHY,CLASS_MUST_BE_ABSTRACT
// JAVAC:UNKNOWN
// 
/* Hierarchy:
 * A class that has (declares or inherits) any abstract methods must be abstract
 * (8.1.1.1, well-formedness constraint 4).
 * 
 * int compareTo(Object o) is still abstract
 */

public class Main implements Comparable {
    public Main() {}
    
    public static int test() {
	return 123;
    }
}
