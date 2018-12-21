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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link ShallowParsedStatementNestingDepthAnalyzer}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 50159 $
 * @ConQAT.Rating GREEN Hash: EFC2B414ED9AB5A4889B5672AA661CF1
 */
public class ShallowParsedStatementNestingDepthAnalyzerTest extends
		TokenTestCaseBase {

	/** Tests LSL calculation without filters. */
	public void testNesting() throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile("Nesting.cs"), ELanguage.CS);
		executeProcessor(ShallowParsedStatementNestingDepthAnalyzer.class,
				"(input=(ref=", element,
				"), findings=(threshold=2.0, key=findings))");

		assertEquals(
				"Violation of Nesting Depth threshold of 2.0: 4.0 @ TEST/Nesting.cs:11-11\n"
						+ "Violation of Nesting Depth threshold of 2.0: 3.0 @ TEST/Nesting.cs:19-19",
				extractFindingsAsString(element));
		assertEquals(
				4.0,
				element.getValue(ShallowParsedStatementNestingDepthAnalyzer.KEY));
	}

	/** Test case for CR#6732. */
	public void testCR6732() throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile("CR6732.js"), ELanguage.JAVASCRIPT);
		executeProcessor(ShallowParsedStatementNestingDepthAnalyzer.class,
				"(input=(ref=", element,
				"), findings=(threshold=3.0, key=findings))");

		assertEquals(
				"Violation of Nesting Depth threshold of 3.0: 4.0 @ TEST/CR6732.js:155-156",
				extractFindingsAsString(element));
	}
}
