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
package org.conqat.engine.dotnet.scope;

import org.conqat.engine.commons.findings.FindingReport;
import org.conqat.engine.commons.findings.util.FindingsReportExtractor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATProcessor;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.engine.resource.text.ITextResource;

/**
 * Base class for test cases testing the report readers on the NUnit test data.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 9B2B31744A0067C441F6D152FB6AED74
 */
public abstract class NUnitReportReaderTestCaseBase extends
		ResourceProcessorTestCaseBase {

	/**
	 * The path where the code was stored at the time the analysis was run, i.e.
	 * the report created.
	 */
	private static final String ANALYSIS_PATH = "z:\\location_unlikely_to_exist\\nunit-2.4.1\\src";

	/** Create a finding report for the given report. */
	public FindingReport obtainFindingReport(String filename,
			Class<? extends IConQATProcessor> reportReaderProcessor)
			throws ConQATException {
		return obtainFindingReport(filename, reportReaderProcessor,
				new LoggerMock());
	}

	/**
	 * Create a finding report for the given report and logs messages to the
	 * given logger.
	 */
	public FindingReport obtainFindingReport(String filename,
			Class<? extends IConQATProcessor> reportReaderProcessor,
			IConQATLogger logger) throws ConQATException {
		ITextResource report = createTextScope(useCanonicalTestFile(""),
				new String[] { filename }, new String[0]);
		ITextResource input = createTextScope(
				useCanonicalTestFile("../org.conqat.engine.dotnet.scope/NUnit_Folder"),
				new String[] { "**/*.cs" }, new String[0]);
		ITextResource element = (ITextResource) executeProcessor(
				reportReaderProcessor, logger, "(input=(ref=", input,
				"),'report-files'=(ref=", report, "),map=(prefix='"
						+ ANALYSIS_PATH
						+ "',project='TEST'), lenient=(mode=true))");
		FindingReport result = (FindingReport) executeProcessor(
				FindingsReportExtractor.class, "(input=(ref=", element, "))");

		return result;
	}
}