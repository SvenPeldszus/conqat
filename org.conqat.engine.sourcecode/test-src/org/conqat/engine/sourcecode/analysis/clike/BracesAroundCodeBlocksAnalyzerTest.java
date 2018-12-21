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
package org.conqat.engine.sourcecode.analysis.clike;

import org.conqat.engine.sourcecode.analysis.FindingsTokenTestCaseBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for {@link BracesAroundCodeBlocksAnalyzer}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49552 $
 * @ConQAT.Rating GREEN Hash: 3C7D97571EF1DD7CDE69ED5CF21EE3FE
 */
public class BracesAroundCodeBlocksAnalyzerTest extends
		FindingsTokenTestCaseBase {

	/** Constructor. */
	public BracesAroundCodeBlocksAnalyzerTest() {
		super(BracesAroundCodeBlocksAnalyzer.class, ELanguage.CPP);
	}

	/** Test analyzer. */
	public void test() throws Exception {
		ITokenElement element = executeProcessor("ConditionsAndLoops.cpp");

		assertFindingCount(element, 6);
		assertFinding(element, 7);
		assertFinding(element, 9);
		assertFinding(element, 14);
		assertFinding(element, 18);
		assertFinding(element, 28);
		assertFinding(element, 32);
	}

	/** Tests the analyzer for statements with precompiled conditions */
	public void testPreCompiledStatements() throws Exception {
		ITokenElement element = executeProcessor("ConditionsAndLoopsWithPrecompilers.cpp");
		assertNoFindings(element);
	}

	/** Tests the analyzer for anonymous classes (see CR#6502). */
	public void testAnonymousClasses() throws Exception {
		ITokenElement element = executeProcessor("AnonymousClasses.java",
				ELanguage.JAVA);
		assertNoFindings(element);
	}

}
