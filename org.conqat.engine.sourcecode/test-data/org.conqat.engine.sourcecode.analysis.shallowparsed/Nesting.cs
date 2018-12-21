namespace Foo {

class Bar {

	public void test () {
	
		for (int i = 0; i < 17; ++i) 
			for (int j = 0; j < 15; ++j) {
				for (int k = 0; k < 1; ++k) 
					if (i < j)
						callFoo();
			}
	
		callMethod (5, x => {
				int a = 17;
				int b = 15;
				return innerCall (a, b, (i,j) => {
					if (i < j)
						return i*j;
					return 0;
				});
			}
		);
	}
}

}
