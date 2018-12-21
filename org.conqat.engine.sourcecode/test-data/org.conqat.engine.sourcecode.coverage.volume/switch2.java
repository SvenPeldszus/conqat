package org.conqat.engine.sourcecode.coverage.volume.java;

@SuppressWarnings("all")
public class SwitchCoverageTest {
	public static void main(String[] args) {
		int digit = 4;
		switch (digit) {
			case 1 :
			case 2 :
			case 3 :
				System.out.println(digit);
		}
	}
}
