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
package org.conqat.engine.java.coverage;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.java.BundleContext;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Base class for testing subclasses of {@link JavaCoverageAnalyzerBase}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48746 $
 * @ConQAT.Rating GREEN Hash: CD5B202591313101B3E1E55DDBD87696
 */
public abstract class JavaCoverageAnalyzerTestBase extends TokenTestCaseBase {

	/** {@inheritDoc} */
	@Override
	protected void setUp() throws Exception {
		createBundleContext(BundleContext.class);
	}

	/** Simple test with single Java class involving an inner class. */
	public void testSimple() throws Exception {
		ITokenResource sourceScope = createJavaResourceHierarchyFor(useTestFile("."));
		runCoverageAnalysis(sourceScope, getSimpleReportFile());
		LineCoverageInfo coverageInfo = (LineCoverageInfo) sourceScope
				.getChildren()[0]
				.getValue(LineCoverageAnalyzerBase.COVERAGE_INFO_KEY);
		assertEquals("[3, 11, 12, 18]", coverageInfo.getFullyCoveredLines()
				.toString());
		assertEquals("[8, 15]", coverageInfo.getPartiallyCoveredLines()
				.toString());
		assertEquals("[9, 22, 26, 27]", coverageInfo.getUncoveredLines()
				.toString());
		assertEquals(.5d, coverageInfo.getCoverageRatio(), .01d);
	}

	/** Template method for retrieving the coverage analyzer under test */
	protected abstract Class<? extends JavaCoverageAnalyzerBase> getCoverageAnalyzerClass();

	/**
	 * Template method for retrieving the file name of the report for the simple
	 * test.
	 */
	protected abstract String getSimpleReportFile();

	/**
	 * Executes the coverage analysis for the given scope and coverage file
	 * within the test data folder
	 */
	protected void runCoverageAnalysis(ITokenResource sourceScope,
			String coverageFile) throws ConQATException {
		ITokenElement coverageReport = createTokenElement(
				useCanonicalTestFile(coverageFile), ELanguage.XML);
		executeProcessor(getCoverageAnalyzerClass(), "(input=(ref=",
				sourceScope, "), 'coverage-reports'=(scope=", coverageReport,
				"))");
	}

}
