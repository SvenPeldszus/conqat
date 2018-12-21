public class A {
	public void a(List<String> x) {
		// there used to be no read generated for "x" here
		for (String a : foo(x)) {
			System.out.println(a);
		}
	}
}