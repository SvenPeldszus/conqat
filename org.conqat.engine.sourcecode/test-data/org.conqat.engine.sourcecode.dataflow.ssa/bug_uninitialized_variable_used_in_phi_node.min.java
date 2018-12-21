// used to raise an AssertionError because a variable was used but not defined
public class A {
	
	/**
	 * Set up the list of external file types, either from default values, or
	 * from values recorded in Preferences.
	 */
	public void updateExternalFileTypes() {
		for (String[] val : vals) {
			if (foo()) {
				for (ExternalFileType type : types) {
				}
			} else {
				ExternalFileType type = bla();
			}
		}
	}

}