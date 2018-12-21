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

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.clone.DeepCloneException;

/**
 * A test container represents a test class that contains test methods that are
 * executed during the test.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: AF6F92887E9A4C8B9FB7CDF7B442DD7A
 */
public class TestContainer extends TestNodeContainer<TestRun, TestMethod> {

	/** Constructor. Adds the container to the provided {@link TestRun}. */
	public TestContainer(TestRun root, String name) {
		super(root, name, TestMethod.class);
	}

	/**
	 * Clone constructor with given parent node.
	 * 
	 * @see TestNodeContainer#TestNodeContainer(TestNodeBase, TestNodeContainer)
	 */
	private TestContainer(TestRun parent, TestContainer testRun)
			throws DeepCloneException {
		super(parent, testRun);
	}

	/** {@inheritDoc} */
	@Override
	public TestContainer deepClone(TestNodeBase parent)
			throws DeepCloneException {
		return new TestContainer(CCSMAssert.checkedCast(parent, TestRun.class),
				this);
	}
}