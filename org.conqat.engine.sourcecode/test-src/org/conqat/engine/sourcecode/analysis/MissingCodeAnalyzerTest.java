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

import org.conqat.engine.sourcecode.analysis.clike.EmptyBlocksAnalyzer;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link EmptyBlocksAnalyzer}.
 *
 * @author $Author: hummelb $
 * @version $Rev: 50600 $
 * @ConQAT.Rating GREEN Hash: C77D60A7596FA759ADFB7ABAC01E1C19
 */
public class MissingCodeAnalyzerTest extends FindingsTokenTestCaseBase {

	/** Constructor. */
	public MissingCodeAnalyzerTest() {
		super(MissingCodeAnalyzer.class, ELanguage.JAVA);
	}

	/** Test analyzer including methods. */
	public void testZeroSLOC() throws Exception {
		ITokenElement element = executeProcessor("NoCode.java");
		assertFindingCount(element, 1);
	}

	/** Test analyzer including methods. */
	public void testHasCode() throws Exception {
		ITokenElement element = executeProcessor("SLOCTestFile01.java");
		assertNoFindings(element);
	}

}