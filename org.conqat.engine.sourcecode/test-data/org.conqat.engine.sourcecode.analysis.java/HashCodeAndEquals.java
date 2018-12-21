package com.acme.foo;

import static org.conqat.lib.scanner.ELanguage.ABAP;

public class HashCodeAndEqualsTest {
	
	public int hashCode() {
		return 0;
	}	
	
	private class InnerClass() {
		
		public boolean equals(Object o) {
			return o instanceof AllEntriesGroup;
		}		
	}
	
}
