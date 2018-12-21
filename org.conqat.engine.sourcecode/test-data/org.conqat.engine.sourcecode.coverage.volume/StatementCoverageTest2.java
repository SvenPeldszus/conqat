package org.conqat.engine.sourcecode.coverage.volume.java;

/**
 * Checks if statements in statements are being counted as well as if
 * declarations are statements.
 */
@SuppressWarnings("all")
public class StatementCoverageTest1 {
	public static void main(String[] args) {
		int b = 3;
		System.out.println(

		increment(b = 4));
	}

	public static int increment(int b) {
		return b + 1;
	}
}
