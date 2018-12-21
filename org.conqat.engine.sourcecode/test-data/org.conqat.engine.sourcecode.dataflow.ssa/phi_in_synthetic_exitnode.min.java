public class A {
	public void a() {
		if (foo()) {
			return;
		}
		int a = 0;
		if (bar()) {
			a = 1;
		}
	}
}