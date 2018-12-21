package org.conqat.engine.sourcecode.coverage.volume.java;

@SuppressWarnings("all")
public class IfElseCoverageTest3 {
	public static void main(String[] args) {
		int digit = 4;
		if (digit > 3) {
			if (digit < 5) {
				System.out.println(digit);
			}
		} else if (digit < 2) {
			System.out.println(digit);
		} else {
			System.out.println(digit);
		}
	}
}
