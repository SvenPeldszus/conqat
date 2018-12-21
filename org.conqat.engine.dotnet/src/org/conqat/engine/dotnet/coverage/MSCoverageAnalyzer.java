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
package org.conqat.engine.dotnet.coverage;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.engine.sourcecode.coverage.ELineCoverage;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * {@ConQAT.Doc}
 * 
 * This processor is similar to {@link CoverageReportReader} but focuses on
 * detailed line information, i.e. determines for each line if it is not
 * covered, partially covered or fully covered (See also
 * {@link LineCoverageInfo}).
 * 
 * @author $Author: streitel $
 * @version $Rev: 51295 $
 * @ConQAT.Rating YELLOW Hash: E462B96AA29C24DAE280A3A18D9638A9
 */
@AConQATProcessor(description = "Parses XML test coverage report files generated "
		+ "by Microsoft coverage tooling and annotates nodes with line coverage "
		+ "information. This processor can merge overlapping coverage reports.")
public class MSCoverageAnalyzer extends LineCoverageAnalyzerBase {

	/** The XML element {@value} . */
	// TODO (FS) why don't you use ECoverageXmlElement?
	private static final String SOURCE_FILE_NAMES_ELEMENT = "SourceFileNames";

	/** The XML element {@value} . */
	private static final String SOURCE_FILE_NAME_ELEMENT = "SourceFileName";

	/** The XML element {@value} . */
	private static final String LINES_ELEMENT = "Lines";

	/** The XML element {@value} . */
	private static final String COVERAGE_ELEMENT = "Coverage";

	/** The XML element {@value} . */
	private static final String SOURCE_FILE_ID_ELEMENT = "SourceFileID";

	/** The XML element {@value} . */
	private static final String LINE_END_ELEMENT = "LnEnd";

	/** The XML element {@value} . */
	private static final String LINE_START_ELEMENT = "LnStart";

	/** List of source code root directories. */
	public List<String> coverageReportSourceDirs = new ArrayList<String>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "report-source", minOccurrences = 1, description = "The root directory "
			+ "of the source code as used in the coverage report. This path will be stripped from all source "
			+ "paths found in the coverage report prior to mapping them to the input source scope.")
	public void addReportSourceDir(
			@AConQATAttribute(name = "root", description = "The source code root directory.") String dir) {
		coverageReportSourceDirs.add(UniformPathUtils
				.normalizeAllSeparators(dir));
	}

	/** Constructor */
	public MSCoverageAnalyzer() {
		super(EnumSet.of(ELanguage.CS, ELanguage.CPP));
	}

	/** {@inheritDoc} */
	@Override
	protected String getQualifiedSourceFileName(ITokenElement element) {
		return UniformPathUtils.stripProject(element.getUniformPath());
	}

	/** {@inheritDoc} */
	@Override
	protected void parseCoverageReport(ITextElement coverageReport)
			throws ConQATException {
		MSCoverageLineCoverageHandler coverageHandler = new MSCoverageLineCoverageHandler();
		TextElementUtils.parseSAX(coverageReport, coverageHandler);
		coverageHandler.storeCoverageInfo();
	}

	/** Handler for processing coverage data */
	private final class MSCoverageLineCoverageHandler extends DefaultHandler {

		/**
		 * Mapping from source file IDs as they occur in the coverage report to
		 * coverage information.
		 */
		private final Map<Integer, LineCoverageInfo> sourceFileIdToCoverage = new HashMap<>();

		/**
		 * Mapping from source file IDs as they occur in the coverage report to
		 * source file paths (relative to {@link #coverageReportSourceDirs}).
		 */
		private final Map<Integer, String> sourceFileIdToSourceFile = new HashMap<>();

		/** Current text content */
		private final StringBuilder textContent = new StringBuilder();

		/** Value of a line start element */
		private int lineStart = -1;

		/** Value of a line end element */
		private int lineEnd = -1;

		/** Value of a SourceFileID element */
		private int sourceFileID = -1;

		/** Value of a Coverage element */
		private int coverage = -1;

		/** Value of a SourceFileName element */
		private String sourceFileName = null;

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) {
			textContent.setLength(0);
		}

		/** {@inheritDoc} */
		@Override
		public void endElement(String uri, String localName, String qName)
				throws SAXException {
			switch (qName) {
			case LINE_START_ELEMENT:
				lineStart = parseCurrentTextAsInt();
				break;
			case LINE_END_ELEMENT:
				lineEnd = parseCurrentTextAsInt();
				break;
			case SOURCE_FILE_ID_ELEMENT:
				sourceFileID = parseCurrentTextAsInt();
				break;
			case COVERAGE_ELEMENT:
				coverage = parseCurrentTextAsInt();
				break;
			case LINES_ELEMENT:
				// TODO (FS) rename to endLinesElement (i.e. with an s at the
				// end of "line")
				endLineElement();
				break;
			case SOURCE_FILE_NAME_ELEMENT:
				sourceFileName = parseNormalizedSourcePath();
				break;
			case SOURCE_FILE_NAMES_ELEMENT:
				sourceFileIdToSourceFile.put(sourceFileID, sourceFileName);
				break;
			}
		}

		/** Ends a line element. */
		private void endLineElement() {
			if (!sourceFileIdToCoverage.containsKey(sourceFileID)) {
				sourceFileIdToCoverage
						.put(sourceFileID, new LineCoverageInfo());
			}
			for (int line = lineStart; line <= lineEnd; line++) {
				sourceFileIdToCoverage.get(sourceFileID).addLineCoverage(line,
						getLineCoverage(coverage));
			}
		}

		/** Parses an int from the current text. */
		private int parseCurrentTextAsInt() throws SAXException {
			try {
				return Integer.parseInt(currentText());
			} catch (NumberFormatException e) {
				throw new SAXException("Error parsing number: "
						+ e.getMessage());
			}
		}

		/**
		 * Normalizes the source file path as it occurs in the coverage report
		 * by normalizing separators and stripping the source dir prefix
		 * {@link MSCoverageAnalyzer#coverageReportSourceDirs}.
		 */
		private String parseNormalizedSourcePath() {
			String normalizedPath = UniformPathUtils
					.normalizeAllSeparators(currentText());

			for (String coverageReportSourceDir : coverageReportSourceDirs) {
				normalizedPath = StringUtils.stripPrefixIgnoreCase(
						normalizedPath, coverageReportSourceDir);
			}

			normalizedPath = StringUtils.stripPrefix(normalizedPath,
					UniformPathUtils.SEPARATOR);
			return normalizedPath;
		}

		/**
		 * Maps the line coverage information in the XML report to the
		 * corresponding {@link ELineCoverage}. The mapping was reverse
		 * engineered from coverage reports.
		 */
		private ELineCoverage getLineCoverage(int coverage) {
			if (coverage == 1) {
				return ELineCoverage.PARTIALLY_COVERED;
			} else if (coverage == 0) {
				return ELineCoverage.FULLY_COVERED;
			}
			return ELineCoverage.NOT_COVERED;
		}

		/** Returns the current text content. */
		private String currentText() {
			return textContent.toString().trim();
		}

		/** {@inheritDoc} */
		@Override
		public void characters(char[] ch, int start, int length) {
			textContent.append(ch, start, length);
		}

		/**
		 * Stores the coverage info via
		 * {@link LineCoverageAnalyzerBase#getOrCreateCoverageInfo}
		 */
		public void storeCoverageInfo() {
			for (Integer sourceFileId : sourceFileIdToCoverage.keySet()) {
				String sourceFile = sourceFileIdToSourceFile.get(sourceFileId);
				if (sourceFile == null) {
					getLogger().error(
							"No source file with the ID '" + sourceFileID
									+ "' found");
					continue;
				}
				LineCoverageInfo coverageInfo = sourceFileIdToCoverage
						.get(sourceFileId);
				getOrCreateCoverageInfo(sourceFile).addAll(coverageInfo);
			}
		}
	}

}
