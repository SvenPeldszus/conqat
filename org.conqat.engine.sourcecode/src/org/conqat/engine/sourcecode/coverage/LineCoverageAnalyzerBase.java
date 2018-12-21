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
package org.conqat.engine.sourcecode.coverage;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementProcessorBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Base class for loading line coverage data.
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 50783 $
 * @ConQAT.Rating YELLOW Hash: CC3EBB29062AF3F678128E1C010AFC7C
 */
public abstract class LineCoverageAnalyzerBase extends
		TokenElementProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The line coverage information", type = "org.conqat.engine.sourcecode.coverage.LineCoverageInfo")
	public static final String COVERAGE_INFO_KEY = "coverage-info";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "coverage-reports", attribute = "scope", description = "The scope with the coverage reports.")
	public ITextResource coverageReportsRoot;

	/**
	 * The coverage data. Maps from qualified source file name to the line
	 * coverage information.
	 */
	private final Map<String, LineCoverageInfo> coverageData = new HashMap<>();

	/** The languages supported by this processor. */
	private final EnumSet<ELanguage> supportedLanguages;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.LOG_LEVEL_NAME, attribute = ConQATParamDoc.ATTRIBUTE_VALUE_NAME, description = "Log level for files without coverage data or unsupported file types. "
			+ ConQATParamDoc.LOG_LEVEL_DESCRIPTION + " [default is WARN]", optional = true)
	public ELogLevel logLevel = ELogLevel.WARN;

	/** Constructor. */
	protected LineCoverageAnalyzerBase(ELanguage supportedLanguage) {
		this.supportedLanguages = EnumSet.of(supportedLanguage);
	}

	/** Constructor. */
	protected LineCoverageAnalyzerBase(EnumSet<ELanguage> supportedLanguages) {
		this.supportedLanguages = supportedLanguages;
	}

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITokenResource root) throws ConQATException {
		NodeUtils.getDisplayList(root).addKey(COVERAGE_INFO_KEY, null);

		List<ITextElement> coverageReports = ResourceTraversalUtils
				.listTextElements(coverageReportsRoot);
		for (ITextElement coverageReport : coverageReports) {
			parseCoverageReport(coverageReport);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		if (!supportedLanguages.contains(element.getLanguage())) {
			getLogger().log(logLevel,
					"Ignored file with unsupported langauge: " + element);
			return;
		}

		String qualifiedSourceFileName = getQualifiedSourceFileName(element);
		LineCoverageInfo coverage = coverageData.get(qualifiedSourceFileName);
		if (coverage == null) {
			getLogger().log(logLevel,
					"No coverage data found for " + qualifiedSourceFileName);
		} else {
			element.setValue(COVERAGE_INFO_KEY, coverage);
		}
	}

	/** Returns the coverage info for the given qualified source name. */
	protected LineCoverageInfo getOrCreateCoverageInfo(
			String qualifiedSourceName) {
		LineCoverageInfo lineCoverageInfo = coverageData
				.get(qualifiedSourceName);
		if (lineCoverageInfo == null) {
			lineCoverageInfo = new LineCoverageInfo();
			coverageData.put(qualifiedSourceName, lineCoverageInfo);
		}
		return lineCoverageInfo;
	}

	/** Returns the qualified source name for an element. */
	protected abstract String getQualifiedSourceFileName(ITokenElement element)
			throws ConQATException;

	/**
	 * Template method for parsing a single coverage report. Subclasses are
	 * expected to fill coverage info obtained via
	 * {@link #getOrCreateCoverageInfo(String)} in this method. This will be
	 * called multiple times, i.e. implementers have to merge coverage data
	 * appropriately.
	 */
	protected abstract void parseCoverageReport(ITextElement coverageReport)
			throws ConQATException;
}
