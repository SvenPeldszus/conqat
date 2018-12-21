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

import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link CoverableMCDCProcessor}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51252 $
 * @ConQAT.Rating GREEN Hash: B526E19C45C57ABB2B2A86E4F00D9D41
 */
public class CoverableMCDCProcessorTest extends TokenTestCaseBase {

	/** Tests with very basic expressions. */
	public void testBasicExpressions() throws Exception {
		assertMcdcHints("foo", ELanguage.JAVA, 2);
		assertMcdcHints("a && b", ELanguage.JAVA, 3);
		assertMcdcHints("a xor b", ELanguage.ADA, 3);
		assertMcdcHints("a and b or c", ELanguage.ADA, 5);
	}

	/** Tests with parentheses. */
	public void testParentheses() throws Exception {
		assertMcdcHints("(a || b) && c", ELanguage.JAVA, 4);
		assertMcdcHints(
				"!(b.getCiteKey().equals(\"canh05\")||b.getCiteKey().equals(\"foo\"))",
				ELanguage.JAVA, 3);
		assertMcdcHints("(flag&flagID)==flagID", ELanguage.JAVA, 2);
	}

	/** Tests error cases encountered in Ada code. */
	public void testAdaProblems() throws Exception {
		assertMcdcHints("R_M.T(1..2)=(C'V(16#30#)&C'V(16#33#))", ELanguage.ADA,
				2);
		assertMcdcHints(
				"(T'F + M_L <= T'L and then (F_F.I_A_7_B_M_C (T (T'F + M_L)) "
						+ "or else (O_P and then S_U.I_A_8_B_N_C (T(T'F + M_L)))))",
				ELanguage.ADA, 6);
	}

	/** Calculates MC/DC test hints and checks their number. Exception */
	private void assertMcdcHints(String expression, ELanguage language,
			int expectedHints) throws Exception {
		ITokenElement element = createTokenElement(
				addScaffolding(expression, language), language);
		executeProcessor(CoverableMCDCProcessor.class, "(input=(ref=", element,
				"))");

		int actualHints = (int) element
				.getValue(CoverableMCDCProcessor.MCDC_TEST_COUNT_KEY);
		if (expectedHints != actualHints) {
			String hints = StringUtils.concat((List<?>) element
					.getValue(CoverableMCDCProcessor.MCDC_TEST_HINTS_KEY),
					StringUtils.CR);
			fail("Expected " + expectedHints + " but had " + actualHints
					+ ". Hints: " + StringUtils.CR + hints);
		}
	}

	/** Wraps the given expression to make it a valid program. */
	private String addScaffolding(String expression, ELanguage language) {
		switch (language) {
		case JAVA:
			return "class A { void foo() { if(" + expression + "){} }}";
		case ADA:
			return "procedure X is begin if " + expression
					+ " then dosomething(); end if; end X;";
		}
		throw new AssertionError("Language not yet supported: " + language);
	}

}
