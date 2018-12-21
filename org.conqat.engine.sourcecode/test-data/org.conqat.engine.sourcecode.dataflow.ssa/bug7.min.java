public class A {
	public void a() {
		Object k = foo();
		Object l = foo();
		if (k == null) {
			if (l == null) {
				return;
			} else {
				return;
			}
		} else if (l == null) {
			return;
		}
		k.toString();
		l.toString();
	}
}