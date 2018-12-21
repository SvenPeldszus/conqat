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
package org.conqat.engine.dotnet.test.mstest;

import java.util.HashMap;

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
import org.joda.time.LocalTime;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Parser that performs the actual XML processing of MSTest TRX files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 98F45A8FD3DBAFA7BF6F8241B4FE54A6
 */
public class TrxFileParser extends TestFileParser {

	/**
	 * URI indicating that code coverage measurement has been enabled (until VS
	 * 2010).
	 */
	private static final String CODE_COVERAGE_URI_1_0 = "datacollector://microsoft/CodeCoverage/1.0";

	/**
	 * URI indicating that code coverage measurement has been enabled (VS 2012+.
	 */
	private static final String CODE_COVERAGE_URI_2_0 = "datacollector://microsoft/CodeCoverage/2.0";

	/** The test run the TRX file describes. */
	protected TestRun run;

	/** Test methods accessible by a unique test id. */
	private final HashMap<String, TestMethod> testMethodsById = new HashMap<>();

	/** Constructor */
	public TrxFileParser(ITextElement trxFile, ResultIdStrategy resultIdStrategy) {
		super(trxFile, resultIdStrategy);
	}

	/** {@inheritDoc} */
	@Override
	protected TestRun parseElement(ITextElement trxFile) throws ConQATException {
		// VisualStudio 2012 Reverted the order of XML elements, hence it is not
		// deterministic if test results are written before a test is defined in
		// XML. Due to this, parsing has been split in extracting the test
		// structure and then extracting the test results.
		// TODO (FS) I would move this inline comment into the method comment

		ElementEnumSaxHandler<ETrxElement> saxHandler = new ElementEnumSaxHandler<>(
				ETrxElement.TESTRUN);
		saxHandler.setElementHandler(ETrxElement.TESTRUN, testRunHandler);
		saxHandler.setElementHandler(ETrxElement.UNITTEST, unitTestHandler);
		saxHandler.setElementHandler(ETrxElement.TESTMETHOD, unitTestHandler);
		TextElementUtils.parseSAX(trxFile, saxHandler);

		saxHandler = new ElementEnumSaxHandler<>(ETrxElement.TESTRUN);
		saxHandler.setElementHandler(ETrxElement.DEPLOYMENT, deploymentHandler);
		saxHandler.setElementHandler(ETrxElement.TIMES, timeHandler);
		saxHandler.setElementHandler(ETrxElement.COLLECTOR, coverageHandler);
		saxHandler
				.setElementHandler(ETrxElement.DATACOLLECTOR, coverageHandler);
		saxHandler.setElementHandler(ETrxElement.UNITTESTRESULT,
				testResultHandler);
		TextElementUtils.parseSAX(trxFile, saxHandler);

		return run;
	}

	/**
	 * Parses the <code>TestRun</code> element to get the {@link TestRun} name.
	 */
	private final ElementHandler<ETrxElement> testRunHandler = new ElementHandler<ETrxElement>() {

		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes) {
			run = new TestRun(getRoot(), attributes.getValue(ETrxAttribute.name
					.name()));
			run.setCodeCoverage(false);
		}
	};

	/**
	 * Parses the <code>UnitTest</code> and
	 * <code>TestMethod</TestMethod> elements and extracts the unit test structure.
	 */
	private final ElementHandler<ETrxElement> unitTestHandler = new ElementHandler<ETrxElement>() {

		/**
		 * The ID of the current test. The ID is specified in the
		 * {@link ETrxElement#UNITTEST} element, whereas the test class and name
		 * are specified in {@link ETrxElement#TESTMETHOD}.
		 */
		private String currentTestId;

		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes) {
			if (element == ETrxElement.UNITTEST) {
				currentTestId = attributes.getValue(ETrxAttribute.id.name());
			} else {
				String className = StringUtils.getFirstParts(
						attributes.getValue(ETrxAttribute.className.name()), 1,
						',');

				TestContainer container = run.getChild(className);
				if (container == null) {
					container = new TestContainer(run, className);
				}

				TestMethod method = new TestMethod(container,
						attributes.getValue(ETrxAttribute.name.name()),
						getResultIdStrategy());

				testMethodsById.put(currentTestId, method);
			}
		}
	};

	/**
	 * Parses the <code>Deployment</code> element to get the working directory.
	 */
	private final ElementHandler<ETrxElement> deploymentHandler = new ElementHandler<ETrxElement>() {
		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes) {
			run.setWorkingDirectory(attributes
					.getValue(ETrxAttribute.runDeploymentRoot.name()));
		}
	};

	/**
	 * Parses the <code>Times</code> element to get the execution time of the
	 * test.
	 */
	private final ElementHandler<ETrxElement> timeHandler = new ElementHandler<ETrxElement>() {
		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes)
				throws SAXException {
			String startDate = attributes.getValue(ETrxAttribute.start.name());
			run.setTime(parseDate(startDate));
		}
	};

	/** Parses information regarding enablement of code coverage. */
	private final ElementHandler<ETrxElement> coverageHandler = new ElementHandler<ETrxElement>() {
		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes) {
			String collectorUri = CODE_COVERAGE_URI_2_0;
			if (element == ETrxElement.DATACOLLECTOR) {
				collectorUri = CODE_COVERAGE_URI_1_0;
			}

			if (collectorUri.equalsIgnoreCase(attributes
					.getValue(ETrxAttribute.uri.name()))) {
				run.setCodeCoverage(true);
			}
		}
	};

	/** Parses the test outcome of a unit test. */
	private final TextElementHandler<ETrxElement> testResultHandler = new TextElementHandler<ETrxElement>() {
		// TODO (FS) missing comment
		TestMethod currentTest;

		/** {@inheritDoc} */
		@Override
		public void onStartElement(ETrxElement element, Attributes attributes)
				throws SAXException {
			String testId = attributes.getValue(ETrxAttribute.testId.name());
			currentTest = testMethodsById.get(testId);

			if (currentTest == null) {
				throw new SAXException("Unknown test with ID: " + testId);
			}

			String outcome = attributes.getValue(ETrxAttribute.outcome.name());
			currentTest.setAssessment(testOutcomeToColor(outcome), outcome);
			parseExecutionTime(attributes, currentTest);
		}

		/** {@inheritDoc} */
		@Override
		public void onText(ETrxElement element, String text) {
			currentTest.addResult(text);
		}

		/**
		 * Parses the test execution time if the {@link ETrxAttribute#duration}
		 * attribute is set and sets it with
		 * {@link TestMethod#setExecutionTime(double)}
		 */
		private void parseExecutionTime(Attributes attributes, TestMethod method)
				throws SAXException {
			String durationString = attributes.getValue(ETrxAttribute.duration
					.name());
			if (durationString == null) {
				return;
			}

			try {
				LocalTime duration = LocalTime.parse(durationString);
				method.setExecutionTime(duration.getMillisOfDay() / 1_000.0);
			} catch (IllegalArgumentException e) {
				throw new SAXException("Unexpected execution time format: "
						+ durationString, e);
			}
		}

		/**
		 * Converts the test outcome string into an {@link ETrafficLightColor}.
		 */
		private ETrafficLightColor testOutcomeToColor(String testOutcome)
				throws SAXException {
			if (testOutcome == null) {
				return ETrafficLightColor.RED;
			}
			switch (testOutcome) {
			case "Passed":
				// TODO (FS) passed but run aborted does not sound very
				// green to me. are we sure this should be green in the
				// dashboard if all tests have that status?
				// TODO (MP) Yea, we had this case for IBNR imho. We agreed
				// with dev that PassedButRunAborted is actually passed.
				// TODO (FS) then please document this rationale in the method
				// comment
			case "PassedButRunAborted":
				return ETrafficLightColor.GREEN;
			case "NotExecuted":
				return ETrafficLightColor.YELLOW;
			case "Failed":
				return ETrafficLightColor.RED;
			default:
				throw new SAXException("Unexpected test outcome: "
						+ testOutcome);
			}
		}
	};
}