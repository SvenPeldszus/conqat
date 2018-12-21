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
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.test.CCSMTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Tests the {@link IConditionExtractor}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51105 $
 * @ConQAT.Rating GREEN Hash: 7CEAB0C736C860CFFD122F38EE5D8460
 */
public class ConditionExtractorTest extends CCSMTestCaseBase {

	/** Test java examples. */
	public void testJava() throws ConQATException {
		assertCondition("boolean a = b;", null, ELanguage.JAVA);
		assertCondition("boolean a = b && c;", "b&&c", ELanguage.JAVA);
		assertCondition("foo(b && c);", "b&&c", ELanguage.JAVA);
		assertCondition("boolean a = x > 5;", "x>5", ELanguage.JAVA);
		assertCondition("boolean a = x > 5 && y < 3;", "x>5&&y<3",
				ELanguage.JAVA);
		assertCondition("boolean a = foo(x > 5) && bar (y < 3) && baz (z);",
				"foo(...)&&bar(...)&&baz(...)", ELanguage.JAVA);
		assertCondition(
				"boolean a = foo<a, b, c>(x > 5 || y > 8) && bar (y < 3)",
				"x>5||y>8", ELanguage.JAVA);
		assertCondition("boolean test = (a || b) && c;", "(a||b)&&c",
				ELanguage.JAVA);
	}

	/** Test Ada examples. */
	public void testAda() throws ConQATException {
		assertCondition("a := b;", null, ELanguage.ADA);
		assertCondition("a := b and c;", "b and c", ELanguage.ADA);
		assertCondition("foo(b and c);", "b and c", ELanguage.ADA);
		assertCondition("a := x > 5;", "x>5", ELanguage.ADA);
		assertCondition("a := x = 5 xor y < 3;", "x=5 xor y<3", ELanguage.ADA);
		assertCondition("a := foo(x > 5) and bar (y < 3) and baz (z);",
				"foo(...) and bar(...) and baz(...)", ELanguage.ADA);
		assertCondition("when 2 =>", null, ELanguage.ADA);
	}

	/** Test C# examples. */
	public void testCs() throws ConQATException {
		assertCondition("string v = n [n.Length-1] == '+' ? option : null;",
				"n[...]=='+'", ELanguage.CS);

		// real world LINQ expression (improves readability a lot)
		// the result in this case is only the first subexpression
		assertCondition(
				"var unprocessed = from argument in arguments where "
						+ "++c.OptionIndex >= 0 && (process || def != null) ? "
						+ "process ? argument == \"--\" ? (process = false) : !Parse (argument, c) ? "
						+ "def != null ? Unprocessed (null, def, c, argument) : true : false : def != null ?"
						+ " Unprocessed (null, def, c, argument) : true : true select argument;",
				"OptionIndex>=0&&(process||def!=null)", ELanguage.CS);
	}

	/**
	 * Asserts that the extracted condition is exactly as expected. Expected
	 * condition may be null.
	 */
	private void assertCondition(String input, String expectedCondition,
			ELanguage language) throws ConQATException {

		List<IToken> tokens = ScannerUtils.getTokens(input, language);
		Condition condition = ConditionParserFactory.createConditionExtractor(
				language).extractGeneralCondition(tokens);
		if (expectedCondition == null) {
			assertNull("Expecting no condition!", condition);
		} else {
			assertNotNull("Expecting condition!", condition);
			assertEquals(expectedCondition, condition.getText());
		}
	}

}
