public class Foo {

	public static void main(String[] args) {
		for (;;) {
			System.err.println();
			if (args.length > 3) {
				break;
			}
		}
	}
}