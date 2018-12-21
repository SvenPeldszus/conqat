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
package org.conqat.engine.text.comments.analysis.finding;

import org.conqat.engine.text.comments.analysis.CommentTestBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link ExclamationQuestionMarkAnalysis}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49736 $
 * @ConQAT.Rating GREEN Hash: 8A8856F22000F3F3F0DC1C9EA4B1D207
 */
public class ExclamationQuestionMarkAnalysisTest extends CommentTestBase {

	/** Tests cases that we want to catch. */
	public void testPositives() throws Exception {
		assertFinding("// This is dangerous!", ELanguage.JAVA,
				ExclamationQuestionMarkAnalysis.class);
		assertFinding("/* What's going on here? */", ELanguage.CS,
				ExclamationQuestionMarkAnalysis.class);
		assertFinding("/** What? No! Yes. */", ELanguage.CS,
				ExclamationQuestionMarkAnalysis.class);
	}

	/** Tests cases that we do not want to catch. */
	public void testNegatives() throws Exception {
		assertNoFinding(
				"/* See https://bugs.eclipse.org/bugs/show_bug.cgi?id=427630 */",
				ELanguage.CPP, ExclamationQuestionMarkAnalysis.class);
		assertNoFinding("// Fail if x != y. */", ELanguage.CPP,
				ExclamationQuestionMarkAnalysis.class);
		assertNoFinding("/* Replace '?' by '!' in next statement */",
				ELanguage.CPP, ExclamationQuestionMarkAnalysis.class);
	}

}
