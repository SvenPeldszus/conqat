package org.conqat.engine.sourcecode.coverage.volume.java;

@SuppressWarnings("all")
public class IfElseCoverageTest1 {
	public static void main(String[] args) {
		int digit = 4;
		if (digit > 3) {
			if (digit < 5) {
				System.out.println("true");
			}
		}
	}
}
