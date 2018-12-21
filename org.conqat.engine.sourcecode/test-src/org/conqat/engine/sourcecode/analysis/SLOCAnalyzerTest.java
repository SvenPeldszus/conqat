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
package org.conqat.engine.sourcecode.analysis;

import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link SLOCAnalyzer}.
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 47919 $
 * @ConQAT.Rating GREEN Hash: 6C85155D2D465F9B38F3CA7A185AE08E
 */
public class SLOCAnalyzerTest extends TokenTestCaseBase {

	/** Test analyzer. */
	public void test() throws Exception {
		assertSLOC("SLOCTestFile01.java", 50, ELanguage.JAVA);
		assertSLOC("SLOCTestFile02.java", 0, ELanguage.JAVA);
		assertSLOC("SLOCTestFile03.java", 0, ELanguage.JAVA);
		assertSLOC("SLOCTestFile04.java", 5, ELanguage.JAVA);
		assertSLOC("SLOCTestFile05.java", 3, ELanguage.JAVA);

		assertSLOC("SLOCTestFile06.cs", 4, ELanguage.CS);
		assertSLOC("SLOCTestFile07.cs", 5, ELanguage.CS);
	}

	/** Checks if the analyzer returns the expected number of SLOCs for a file. */
	private void assertSLOC(String filename, double expectedSLOC,
			ELanguage language) throws Exception {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), language);
		executeProcessor(SLOCAnalyzer.class, "(input=(ref=", element, "))");
		assertEquals(expectedSLOC, element.getValue(SLOCAnalyzer.KEY));
	}
}