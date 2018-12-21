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
package org.conqat.engine.sourcecode.report;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.commons.findings.Finding;
import org.conqat.engine.commons.findings.FindingReport;
import org.conqat.engine.commons.findings.util.FindingUtils;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.base.ReportReaderBase;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Base class for report reader tests.
 *
 * @author $Author: heinemann $
 * @version $Rev: 49956 $
 * @ConQAT.Rating GREEN Hash: DB0ADD3A29673A0F3D40F8C9A43E80C8
 */
public abstract class ReportReaderTestBase extends TokenTestCaseBase {

	/**
	 * Runs the given report reader class and returns a string representation of
	 * the findings loaded.
	 *
	 * @param additionalParameters
	 *            any additional parameters to include in CQDDL for running the
	 *            report reader (may be null).
	 * @return The findings, each on a separate line, sorted alphabetically.
	 */
	protected String runReportReaderAndReturnFindings(
			Class<? extends ReportReaderBase> processorClass,
			String reportIncludePattern, String additionalParameters)
			throws ConQATException {
		ITextResource system = createTextScope(useTestFile("system"),
				getIncludePatterns(), null);
		ITextResource reports = createTextScope(useTestFile("reports"),
				new String[] { reportIncludePattern }, null);

		if (StringUtils.isEmpty(additionalParameters)) {
			additionalParameters = StringUtils.EMPTY_STRING;
		} else if (!additionalParameters.startsWith(",")) {
			additionalParameters = ", " + additionalParameters;
		}

		executeProcessor(processorClass, "(input=(ref=", system,
				"), 'report-files'=(ref=", reports,
				"), 'category-name'=(value=CategoryName)"
						+ additionalParameters + ")");

		FindingReport findingReport = NodeUtils.getFindingReport(system);

		List<String> findings = new ArrayList<String>();
		for (Finding finding : FindingUtils.getAllFindings(findingReport)) {
			findings.add(finding.getMessage() + "@"
					+ finding.getLocationString());
		}
		return StringUtils.concat(CollectionUtils.sort(findings), "\n");
	}

	/** Return the include patterns used for the text scope */
	protected abstract String[] getIncludePatterns();

}
