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
 * Test for {@link JavaHashCodeAndEqualsAnalyzer}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50499 $
 * @ConQAT.Rating GREEN Hash: 89EEE3AA27E0410E9576DDC1D8ED02C6
 */
public class JavaHashCodeAndEqualsAnalyzerTest extends
		FindingsTokenTestCaseBase {

	/** Constructor. */
	public JavaHashCodeAndEqualsAnalyzerTest() {
		super(JavaHashCodeAndEqualsAnalyzer.class, ELanguage.JAVA);
	}

	/**
	 * Test whether the analyzer finds missing methods in classes and inner
	 * classes
	 */
	public void testWithInnerClass() throws Exception {
		ITokenElement element = executeProcessor("HashCodeAndEquals.java");

		assertFindingCount(element, 2);
		assertFinding(element, 7, 7);
		assertFinding(element, 13, 13);
	}
}