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
package org.conqat.engine.dotnet.test.mstest;

import org.conqat.engine.dotnet.test.DotNetTestResultReaderBase;
import org.conqat.engine.dotnet.test.DotNetTestResultReaderTestBase;
import org.conqat.engine.dotnet.test.TestRoot;
import org.conqat.lib.commons.assessment.ETrafficLightColor;

/**
 * Test for the {@link TrxFileReader}.
 * 
 * @author Martin Feilkas
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: A0E4AE6F16E0C2E9758B9F3491464784
 */
// TODO (FS) the comparators and the package are named MSTest..., but here the
// classes are all named Trx... please consolidate
public class TrxFileReaderTest extends DotNetTestResultReaderTestBase {

	/** Test the TrxFileReader with all trx-files in the test-data directory. */
	public void testTrxFileReader() throws Exception {

		TestRoot root = readTestResult();

		assertEquals(3, root.getChildren().length);

		assertTestResult(root, 27, 2, 4);

		assertCoverageEnablement(
				root.getChild("juergens@JUERGENS-LAPTOP 2011-09-08 13:23:20"),
				true);
		assertCoverageEnablement(
				root.getChild("juergens@JUERGENS-LAPTOP 2011-09-08 13:23:20 2013"),
				true);
		assertCoverageEnablement(
				root.getChild("ser-dev 2010-05-25 10:17:31_ANY CPU_Release"),
				false);

		assertDeepClone(root);
	}

	/** {@inheritDoc} */
	@Override
	protected ETrafficLightColor getExpectedTestColor(String testName) {
		switch (testName) {
		case "TestCoverageResponsibilityQuery":
		case "TestFail":
		case "TestExceptionWithoutOutcome":
			return ETrafficLightColor.RED;
		case "TestFormatTypeString":
			return ETrafficLightColor.YELLOW;
		default:
			return ETrafficLightColor.GREEN;
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String getExpectedTestResult(String testName) {
		switch (testName) {
		case "TestCoverageResponsibilityQuery":
			return "Failed";
		case "TestFail":
			return "Failed\n\nAssert.IsTrue failed. \n"
					+ "at UnitTests.UnitTest1.TestFail() in E:\\code\\cqse-conqat-root\\engine\\eu.cqse.conqat.systemtest\\test-src-cs\\main\\SystemtestSolution\\UnitTests\\UnitTest1.cs:line 36";
		case "TestFormatTypeString":
			return "NotExecuted";
		case "TestExceptionWithoutOutcome":
			return "Unit Test Adapter threw exception:\n"
					+ "Type is not resolved for member 'Foo.Bar....'..";
		case "TestSuccess":
			return "PassedButRunAborted";
		case "TestCallStaticMethod":
			return "Passed\n\nStatic method called";
		default:
			return "Passed";
		}
	}

	/** {@inheritDoc} */
	@Override
	protected DotNetTestResultReaderBase createTestFileReader() {
		return new TrxFileReader();
	}
}