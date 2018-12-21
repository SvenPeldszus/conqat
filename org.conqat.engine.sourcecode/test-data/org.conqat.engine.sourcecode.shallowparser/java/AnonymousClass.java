public class AnonymousClass {

	private final IRunnable r = new IRunnable() {
		public void run() {
			System.out.println("Run1");
		}
	};

	public void foo() {
		new Thread(new IRunnable() {
			public void run() {
				System.out.println("Run1");
			}
		}).start();
	}

	// find anon classes also in loop constructs
	public void ugly1() {
		for (int i = 0; new Comparator<Double, Double>() {
			@Override
			public int compare(Double d1, Double d2) {
				return 17;
			}
		}.compare(.1, .2) > 0; ++i) {
			System.out.println("foo");
		}
	}

	// find anon classes also in constructor args to other anon classes
	public Object ugly2() {
		return new FactoryBase(new IFactoryParameter() {
			public int fancyMethod() {
				return 17;
			}
		}) {

			public Object create() {
				return "foo";
			}

		}.create();
	}

}
