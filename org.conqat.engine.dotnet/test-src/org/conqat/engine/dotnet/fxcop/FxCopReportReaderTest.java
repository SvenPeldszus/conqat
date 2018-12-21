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
package org.conqat.engine.dotnet.fxcop;

import org.conqat.engine.commons.findings.FindingReport;
import org.conqat.engine.commons.findings.FindingTestUtils;
import org.conqat.engine.commons.findings.location.ElementLocation;
import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.core.logging.testutils.CollectingLogger;
import org.conqat.engine.dotnet.scope.NUnitReportReaderTestCaseBase;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Test for the {@link FxCopReportReader}.
 * 
 * TODO (FD): This does not test the new type-based mapping as the input is a
 * text not a token scope. Maybe the FxCop reader should only work for token
 * scopes.
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 50677 $
 * @ConQAT.Rating RED Hash: 9ED11AB2AA511E6CB73F165AB63E055F
 */
// TODO (FS) this file contains a TODO by FD, please resolve
// TODO (MP) I know, leave this TODO out of the CR
public class FxCopReportReaderTest extends NUnitReportReaderTestCaseBase {

	/** The logger used for executing the tests. */
	private CollectingLogger logger;

	/**
	 * The log messages while analyzing 'dependency-error-report.xml' with
	 * dependency issues.
	 */
	private static final String[] DEPENDENCY_ERROR_LOG_MESSAGES = new String[] {
			"FxCop threw an exception while writing dependency-error-report.xml"
					+ StringUtils.CR
					+ "Could not load X.Y.Z.dll."
					+ StringUtils.CR
					+ "The following error was encountered while reading module 'X.Y.Z': Assembly reference cannot be resolved: stdole, Version=7.0.3300.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a.",
			"FxCop threw an exception while writing dependency-error-report.xml"
					+ StringUtils.CR + "No targets were selected.",
			"FxCop threw an exception while writing dependency-error-report.xml"
					+ StringUtils.CR
					+ "The referenced assembly 'stdole, Version=7.0.3300.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a' could not be found. This assembly is required for analysis and was referenced by: X.Y.Z.dll." };

	/** {@inheritDoc} */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		logger = new CollectingLogger(ELogLevel.ERROR);
	}

	/** Test the analyzer with the NUint test data. */
	public void testWithNUnit() throws Exception {

		FindingReport report = obtainFindingReport("nunit-fxcop-report.xml",
				FxCopReportReader.class, logger);

		assertEquals(963, FindingTestUtils.countFindings(report));
		assertEquals(963, FindingTestUtils.countLocations(report,
				TextRegionLocation.class));
		assertEquals(0,
				FindingTestUtils.countLocations(report, ElementLocation.class));
	}

	/** Test the analyzer with a report containing dependency errors. */
	public void testWithDependencyErrorReport() throws Exception {

		FindingReport report = obtainFindingReport(
				"dependency-error-report.xml", FxCopReportReader.class, logger);

		assertEquals(0, FindingTestUtils.countFindings(report));
		UnmodifiableList<String> messages = logger.getMessages();
		assertEquals(DEPENDENCY_ERROR_LOG_MESSAGES.length, messages.size());
		for (int i = 0; i < DEPENDENCY_ERROR_LOG_MESSAGES.length; i++) {
			assertEquals(DEPENDENCY_ERROR_LOG_MESSAGES[i], messages.get(i));
		}
	}
}