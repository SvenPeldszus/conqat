public class A {
	public void a() {
		Object h = foo();
		if (h == null) {
			h.toString();
		}
		// there used to be a stray h_-3 in the conditional
		// because the minimizer did not do alias replacement in conditionals
		if (h != null) {
			h.toString();
		}
	}
}