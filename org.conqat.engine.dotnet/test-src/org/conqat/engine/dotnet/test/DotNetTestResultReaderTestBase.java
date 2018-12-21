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
package org.conqat.engine.dotnet.test;

import java.util.List;
import java.util.Map;

import org.conqat.engine.commons.traversal.TraversalUtils;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.ProcessorInfoMock;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.clone.DeepCloneException;

/**
 * Test base class for the classes implementing
 * {@link DotNetTestResultReaderBase}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: C1D41B37B51E22B3687639D704D49C46
 */
public abstract class DotNetTestResultReaderTestBase extends
		ResourceProcessorTestCaseBase {

	/**
	 * Asserts that the test root contains the given amount of passed, failed
	 * and ignored tests.
	 */
	// TODO (FS) I'm still not satisfied with the method comment. please
	// document that this also checks the result string
	protected void assertTestResult(TestRoot root, int expectedPassed,
			int expectedIgnored, int expectedFailed) throws Exception {
		List<TestMethod> results = root.listSortedTestMethods();

		int passed = 0;
		int ignored = 0;
		int failed = 0;

		for (TestMethod result : results) {
			ETrafficLightColor color = getExpectedTestColor(result.getName());

			if (color == ETrafficLightColor.GREEN) {
				passed++;
			} else if (color == ETrafficLightColor.YELLOW) {
				ignored++;
			} else if (color == ETrafficLightColor.RED) {
				failed++;
			}

			assertEquals("Unexpected result for test " + result.getName(),
					getExpectedTestResult(result.getName()), result.getResult());

			assertEquals("Unexpected assessment for test " + result.getName(),
					new Assessment(color), result.getAssessment());
		}

		// compare test results
		int expectedSum = expectedFailed + expectedIgnored + expectedPassed;
		assertEquals(expectedSum, results.size());
		assertEquals(expectedSum, failed + passed + ignored);
		assertEquals(expectedPassed, passed);
		assertEquals(expectedFailed, failed);
		assertEquals(expectedIgnored, ignored);
	}

	/** Asserts that the code coverage is enabled or disabled for a test run. */
	protected void assertCoverageEnablement(TestRun testRun, boolean enabled) {
		assertNotNull(testRun);
		assertEquals(enabled, testRun.isCodeCoverageEnabled());
	}

	/**
	 * Creates the test result scope by running the processor implementing
	 * {@link DotNetTestResultReaderBase}.
	 */
	protected TestRoot readTestResult() throws ConQATException {
		// execute test file reader processor
		DotNetTestResultReaderBase reader = createTestFileReader();
		reader.init(new ProcessorInfoMock());

		ITextResource root = createTextScope(useTestFile("/"),
				new String[] { "**" }, new String[] { "**.result.txt" });
		reader.setInput(root);
		TestRoot rootNode = reader.process();
		return rootNode;
	}

	/** Asserts that deep cloning produces new nodes with different parents. */
	protected void assertDeepClone(TestNodeBase root) throws DeepCloneException {
		TestNodeBase clone = root.deepClone();

		Map<String, TestNodeBase> rootMap = TraversalUtils
				.createIdToNodeMap(root);
		Map<String, TestNodeBase> cloneMap = TraversalUtils
				.createIdToNodeMap(clone);

		for (String id : rootMap.keySet()) {
			// TODO (FS) use assertNotSame?
			assertFalse("Cloned node " + id + " identical to origin",
					rootMap.get(id) == cloneMap.get(id));
			if (rootMap.get(id).getParent() != null) {
				// TODO (FS) use assertNotSame?
				assertFalse("Cloned node parent " + id
						+ " identical to origin parent", rootMap.get(id)
						.getParent() == cloneMap.get(id).getParent());
			}
		}
	}

	/**
	 * Creates a new instance of the {@link DotNetTestResultReaderBase} that is
	 * under test.
	 */
	protected abstract DotNetTestResultReaderBase createTestFileReader();

	/** Returns the expected test outcome color for the given test name. */
	protected abstract ETrafficLightColor getExpectedTestColor(String testName);

	/** Returns the expected test result for the given test name. */
	protected abstract String getExpectedTestResult(String testName)
			throws Exception;
}