public class Foo {

	public static void main(String[] args) {
		System.out.println("hello world!");
	}

	public static enum MyEnum {

		/** Unrelated stuff a. */
		LABEL1,

		/** A label. */
		LABEL2,

		/** Unrelated stuff b. */
		LABEL3;

		public String toString() {
			return "hiho";
		}
	}

}