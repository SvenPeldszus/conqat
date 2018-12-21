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
package org.conqat.engine.sourcecode.analysis.cs;

import org.conqat.engine.sourcecode.analysis.FindingsTokenTestCaseBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link CSHashCodeAndEqualsAnalyzer}.
 *
 * @author $Author: heinemann $
 * @version $Rev: 51200 $
 * @ConQAT.Rating GREEN Hash: 2E29D0CA8574D1CB8C02CB407B27C52B
 */
public class CSHashCodeAndEqualsAnalyzerTest extends FindingsTokenTestCaseBase {

	/** Constructor. */
	public CSHashCodeAndEqualsAnalyzerTest() {
		super(CSHashCodeAndEqualsAnalyzer.class, ELanguage.CS);
	}

	/**
	 * Test whether the analyzer finds missing methods in classes and inner
	 * classes
	 */
	public void testWithInnerClass() throws Exception {
		ITokenElement element = executeProcessor("HashCodeAndEquals.cs");

		assertFindingCount(element, 2);
		assertFinding(element, 3, 3);
		assertFinding(element, 10, 10);
	}

	/**
	 * Test whether the analyzer does not crash on code using preprocessor
	 * directives. See also: CR#7032
	 */
	public void testPreprocessor() throws Exception {
		ITokenElement element = executeProcessor("Preprocessor.cs");
		assertNoFindings(element);
	}

}