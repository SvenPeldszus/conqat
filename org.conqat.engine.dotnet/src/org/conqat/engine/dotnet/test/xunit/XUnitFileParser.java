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
package org.conqat.engine.dotnet.test.xunit;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.test.TestContainer;
import org.conqat.engine.dotnet.test.TestFileParser;
import org.conqat.engine.dotnet.test.TestMethod;
import org.conqat.engine.dotnet.test.TestRun;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.engine.sourcecode.test.TestResultReaderBase.ResultIdStrategy;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.xml.ElementEnumSaxHandler;
import org.conqat.lib.commons.xml.ElementEnumSaxHandler.ElementHandler;
import org.conqat.lib.commons.xml.ElementEnumSaxHandler.TextElementHandler;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Parser that performs the actual XML processing of xUnit TRX files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 084A570EBAAB91CB731C9D4A8A3D6479
 */
// TODO (FS) I thought MSTest had TRX files?
public class XUnitFileParser extends TestFileParser {

	/** The test run the xUnit file describes. */
	private TestRun run;

	/** Constructor */
	public XUnitFileParser(ITextElement xUnitFile,
			ResultIdStrategy resultIdStrategy) {
		super(xUnitFile, resultIdStrategy);
	}

	/**
	 * Parses the relevant data from the XUnit report and returns its content as
	 * a {@link TestRun}.
	 */
	@Override
	public TestRun parseElement(ITextElement xUnitFile) throws ConQATException {
		ElementEnumSaxHandler<EXUnitElement> saxHandler = new ElementEnumSaxHandler<>(
				EXUnitElement.ASSEMBLY);
		saxHandler.setElementHandler(EXUnitElement.ASSEMBLY, assemblyHandler);
		saxHandler.setElementHandler(EXUnitElement.TEST, testHandler);

		TextElementUtils.parseSAX(xUnitFile, saxHandler);

		return run;
	}

	/** Element handler for the <code>assembly</code> element. */
	// TODO (FS) please also describe what the handler DOES when it encounters
	// the element
	private final ElementHandler<EXUnitElement> assemblyHandler = new ElementHandler<EXUnitElement>() {
		// TODO (FS) missing comment
		@Override
		public void onStartElement(EXUnitElement element, Attributes attributes)
				throws SAXException {
			run = new TestRun(getRoot(), getAttribute(attributes,
					EXUnitAttribute.NAME));

			String runTime = getAttribute(attributes, EXUnitAttribute.RUN_TIME);
			String runDate = getAttribute(attributes, EXUnitAttribute.RUN_DATE);
			run.setTime(parseDate(runDate + "T" + runTime));
		}
	};

	/** Element handler for the <code>test</code> element. */
	// TODO (FS) please also describe what the handler DOES when it encounters
	// the element
	private final TextElementHandler<EXUnitElement> testHandler = new TextElementHandler<EXUnitElement>() {

		/** The test result that is currently being parsed. */
		protected TestMethod currentResult;

		// TODO (FS) missing comment
		@Override
		public void onStartElement(EXUnitElement element, Attributes attributes)
				throws SAXException {
			String testClass = getAttribute(attributes, EXUnitAttribute.TYPE);
			TestContainer container = run.getChild(testClass);
			if (container == null) {
				container = new TestContainer(run, testClass);
			}

			// We cannot use the test method attribute as it does not
			// contain optional test parameters, hence we have to construct
			// the test name from the fully qualified method name minus the
			// class name.
			String qualifiedTestName = getAttribute(attributes,
					EXUnitAttribute.NAME);
			String testName = StringUtils.stripPrefix(qualifiedTestName,
					testClass + ".");
			currentResult = new TestMethod(container, testName,
					getResultIdStrategy());

			String result = getAttribute(attributes, EXUnitAttribute.RESULT);
			currentResult.setAssessment(testOutcomeToColor(result), result);

			String timeString = getAttribute(attributes, EXUnitAttribute.TIME);
			if (!StringUtils.isEmpty(timeString)) {
				currentResult.setExecutionTime(Double.parseDouble(timeString));
			}
		}

		// TODO (FS) missing comment
		@Override
		public void onText(EXUnitElement element, String text) {
			currentResult.addResult(text);
		}

		/**
		 * Converts the test outcome string into an {@link ETrafficLightColor}.
		 */
		private ETrafficLightColor testOutcomeToColor(String testOutcome)
				throws SAXException {
			switch (testOutcome) {
			case "Pass":
				return ETrafficLightColor.GREEN;
			case "Skip":
				return ETrafficLightColor.YELLOW;
			case "Fail":
				return ETrafficLightColor.RED;
			default:
				throw new SAXException("Unexpected test outcome: "
						+ testOutcome);
			}
		}
	};

	/**
	 * Returns the attribute with the lowercased name of the enumeration values,
	 * where underscores are replaced by dashes.
	 */
	// TODO (FS) could be made private
	protected static String getAttribute(Attributes attributes, Enum<?> name) {
		return attributes.getValue(name.name().toLowerCase().replace('_', '-'));
	}
}