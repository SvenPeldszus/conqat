public class Foo {

	public static void main(String[] args) {
		for (int i : Arrays.asList(1, 2, 3)) {
			System.err.println();
			if (args.length > 3) {
				break;
			}
		}
	}
}