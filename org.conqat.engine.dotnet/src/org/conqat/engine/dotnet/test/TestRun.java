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
package org.conqat.engine.dotnet.test;

import java.util.Date;

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.clone.DeepCloneException;

/**
 * This class represents a single run of a test tool and holds
 * {@link TestContainer}s.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 4F7C578FE9BE0CE5144AFFF689204B59
 */
// TODO (FS) TestContainer and TestNodeContainer are hard to distinguish. I
// would suggest renaming one of them. I already confused them when reading this
// comment
public class TestRun extends TestNodeContainer<TestRoot, TestContainer> {

	/** Constructor. Adds the test run to the provided {@link TestRoot}. */
	public TestRun(TestRoot root, String name) {
		super(root, name, TestContainer.class);
	}

	/**
	 * Clone constructor with given parent node.
	 * 
	 * @see TestNodeContainer#TestNodeContainer(TestNodeBase, TestNodeContainer)
	 */
	private TestRun(TestRoot parent, TestRun testRun) throws DeepCloneException {
		super(parent, testRun);
	}

	/** {@inheritDoc} */
	@Override
	public TestRun deepClone(TestNodeBase parent) throws DeepCloneException {
		return new TestRun(CCSMAssert.checkedCast(parent, TestRoot.class), this);
	}

	/**
	 * Sets the directory the tests were run in. Sets
	 * {@value DotNetTestResultReaderBase#WORKING_DIRECTORY_KEY}.
	 */
	public void setWorkingDirectory(String directory) {
		setValue(DotNetTestResultReaderBase.WORKING_DIRECTORY_KEY, directory);
	}

	/**
	 * Gets the directory the test were run in or <code>null</code>. Gets
	 * {@value DotNetTestResultReaderBase#WORKING_DIRECTORY_KEY}.
	 */
	public String getWorkingDirectory() {
		return NodeUtils.getStringValue(this,
				DotNetTestResultReaderBase.WORKING_DIRECTORY_KEY, null);
	}

	/**
	 * Sets the code coverage flag. Sets
	 * {@value DotNetTestResultReaderBase#CODE_COVERAGE}.
	 */
	public void setCodeCoverage(boolean enabled) {
		setValue(DotNetTestResultReaderBase.CODE_COVERAGE, enabled);
	}

	/**
	 * Returns <code>true</code> if code coverage measurement was enabled during
	 * a test run. Gets {@value DotNetTestResultReaderBase#CODE_COVERAGE}.
	 */
	public boolean isCodeCoverageEnabled() {
		return NodeUtils.getValue(this,
				DotNetTestResultReaderBase.CODE_COVERAGE, Boolean.class, false);
	}

	/**
	 * Sets the time of the test run. Sets
	 * {@link DotNetTestResultReaderBase#TIME_KEY}.
	 */
	public void setTime(Date time) {
		setValue(DotNetTestResultReaderBase.TIME_KEY, time);
	}

	/**
	 * Returns the time of the test run or <code>null</code>. Gets
	 * {@link DotNetTestResultReaderBase#TIME_KEY}.
	 */
	public Date getTime() {
		return NodeUtils.getValue(this, DotNetTestResultReaderBase.TIME_KEY,
				Date.class, null);
	}
}
