/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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
package org.conqat.engine.sourcecode.analysis.java;

import org.conqat.engine.sourcecode.analysis.FindingsTokenTestCaseBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link JavaPublicAttributesAnalyzer}.
 *
 * @author $Author: hummelb $
 * @version $Rev: 51290 $
 * @ConQAT.Rating GREEN Hash: 439D74B11484D53ED25B1DB7B0CF0EB1
 */
public class JavaPublicAttributesAnalyzerTest extends FindingsTokenTestCaseBase {

	/** Constructor. */
	public JavaPublicAttributesAnalyzerTest() {
		super(JavaPublicAttributesAnalyzer.class, ELanguage.JAVA);
	}

	/**
	 * Tests whether all public fields are found
	 */
	public void testWithoutFilter() throws Exception {
		ITokenElement element = executeProcessor("PublicAttributes.java");
		assertFindingCount(element, 4);
	}

	/**
	 * Tests if all public fields with an AConQATFieldParameter annotation are
	 * ignored
	 */
	public void testWithFilter() throws Exception {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile("PublicAttributes.java"), ELanguage.JAVA);
		executeProcessor(processor, "(input=(ref=", element,
				"), annotations=(ignore='", "@AConQATFieldParameter", "'))");

		assertFindingCount(element, 1);
	}
}
