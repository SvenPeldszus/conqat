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
package org.conqat.engine.dotnet.test.xunit;

import java.io.IOException;

import org.conqat.engine.dotnet.test.DotNetTestResultReaderBase;
import org.conqat.engine.dotnet.test.DotNetTestResultReaderTestBase;
import org.conqat.engine.dotnet.test.TestRoot;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Test for the {@link XUnitFileReader}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: A876E1CA978344B6CB7BEDFDEE826E1B
 */
public class XUnitFileReaderTest extends DotNetTestResultReaderTestBase {

	/**
	 * Test the {@link XUnitFileReader} with all xunit-xml-files in the
	 * test-data directory.
	 */
	public void testXUnitFileReader() throws Exception {
		TestRoot root = readTestResult();
		assertTestResult(root, 4, 1, 1);
		assertDeepClone(root);
	}

	/** {@inheritDoc} */
	@Override
	protected ETrafficLightColor getExpectedTestColor(String testName) {
		switch (testName) {
		case "FailToAddTwoNumbers":
			return ETrafficLightColor.RED;
		case "AddingSeveralNumbers(firstNumber: \"60\", secondNumber: \"70\", result: \"130\", exampleTags: System.String[])":
			return ETrafficLightColor.YELLOW;
		default:
			return ETrafficLightColor.GREEN;
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String getExpectedTestResult(String testName) throws IOException {
		testName = testName.replaceAll("[,:(]", "_");
		testName = testName.replaceAll("\\W+", StringUtils.EMPTY_STRING);

		return FileSystemUtils.readFile(useTestFile(testName + ".result.txt"));
	}

	/** {@inheritDoc} */
	@Override
	protected DotNetTestResultReaderBase createTestFileReader() {
		return new XUnitFileReader();
	}
}