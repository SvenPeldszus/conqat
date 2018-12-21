/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.shallowparser;

import java.util.List;

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.test.CCSMTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Tests the {@link TokenStreamUtils}.
 *
 * @author $Author: hummelb $
 * @version $Rev: 51772 $
 * @ConQAT.Rating GREEN Hash: DDE8D331D04CA998D9ABC8BE6858DCE4
 */
public class TokenStreamUtilsTest extends CCSMTestCaseBase {

	/**
	 * Tests the return type selection using
	 * {@link TokenStreamUtils#getType(List, int)}.
	 */
	public void testReturnTypes() {
		// arrays
		typeMatches("int[]", "public int[] foo() {};", "foo");
		typeMatches("int[][]", "public int[][] foo() {};", "foo");

		// Parameterized
		typeMatches("List<String>", "public List<String> foo() {};", "foo");
		typeMatches("List", "public List foo() {};", "foo");
		typeMatches("List<List<String>>",
				"public List<List<String>> foo() {};", "foo");
		typeMatches("List<int[]>", "public List<int[]> foo() {};", "foo");
		typeMatches("int[]", "int[] foo;", "foo");
		typeMatches("int", "int foo;", "foo");
		typeMatches("List<Integer>", "List<Integer> foo;", "foo");

		// Fully qualified types
		typeMatches("java.lang.Integer[][]",
				"public java.lang.Integer[][] foo() {};", "foo");

		typeMatches("List<java.lang.Integer>", "List<java.lang.Integer> foo;",
				"foo");

		typeMatches("java.util.List<java.lang.Integer>",
				"java.util.List<java.lang.Integer> foo;", "foo");

		typeMatches("java.util.List<java.lang.Integer>",
				"public java.util.List<java.lang.Integer> foo() {};", "foo");

		typeMatches("java.lang.Integer", "public java.lang.Integer foo() {};",
				"foo");
	}

	/**
	 * Checks if the returned type using
	 * {@link TokenStreamUtils#getType(List, int)} returns the expected value.
	 */
	private void typeMatches(String expected, String input, String methodName) {
		List<IToken> tokens = ScannerUtils.getTokens(input, ELanguage.JAVA);
		int nameIndex = -1;
		for (IToken token : tokens) {
			if (token.getText().equals(methodName)) {
				nameIndex = tokens.indexOf(token);
				break;
			}
		}
		CCSMAssert.isTrue(nameIndex > 0, "Method name token index must be > 0");

		String type = TokenStreamUtils.getType(tokens, nameIndex);
		assertEquals("Types don't match", expected, type);
	}
}
