public class Foo {

	public static void main(String[] args) {

		for (int i = 0; i < 15; ++i) 
		{
			System.out.println();
		}

		do {
			System.err.println();
		} while (args.length < 1);

		while (args.length < 3) {
			System.err.println();
		}
	}
}