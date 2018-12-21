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
package org.conqat.engine.sourcecode.analysis.javascript;

import org.conqat.engine.sourcecode.analysis.FindingsTokenTestCaseBase;
import org.conqat.engine.sourcecode.analysis.clike.BracesAroundCodeBlocksAnalyzer;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link BracesAroundCodeBlocksAnalyzer}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48675 $
 * @ConQAT.Rating GREEN Hash: CA3424F1ABDDDF670FB930BCE0E976CC
 */
public class BracesAroundCodeBlocksAnalyzerTest extends
		FindingsTokenTestCaseBase {

	/** Constructor. */
	public BracesAroundCodeBlocksAnalyzerTest() {
		super(BracesAroundCodeBlocksAnalyzer.class, ELanguage.JAVASCRIPT);
	}

	/** Test analyzer. */
	public void test() throws Exception {
		ITokenElement element = executeProcessor("EmptyBlocks.js");
		assertFinding(element, 8);
		assertFindingCount(element, 1);
	}
}
