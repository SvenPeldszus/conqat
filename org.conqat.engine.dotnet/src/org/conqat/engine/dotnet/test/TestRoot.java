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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.clone.DeepCloneException;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Holds the results of a unit test execution.
 * <p>
 * A {@link TestRoot} contains one or more {@link TestRun}s that summarize
 * {@link TestMethod}s which are executed by one call to the test tool. The
 * reason for organizing {@link TestRun}s in a {@link TestRoot} is that the
 * overall test result may consist of several executions of one (or different)
 * test tools.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 81E1E1EAFF142E7F5D4E466ACC03B78B
 */
// TODO (FS) comment does not mention the role of test containers
public class TestRoot extends TestNodeContainer<TestNodeBase, TestRun> {

	/** Constructor. */
	public TestRoot() {
		super(null, StringUtils.EMPTY_STRING, TestRun.class);
	}

	/** Clone constructor. */
	private TestRoot(TestRoot root) throws DeepCloneException {
		super(null, root);
	}

	/** Returns a list of all test methods. */
	public List<TestMethod> listTestMethods() {
		List<TestMethod> results = new ArrayList<>();

		// TODO (FS) use TraversalUtils#listLeaves and retype result? to retype,
		// you can reuse GenealogyElementContainerBase#retype. I would like it
		// if you move that method to CollectionUtils and reuse it here
		for (TestRun run : getChildren()) {
			for (TestContainer container : run.getChildren()) {
				for (TestMethod result : container.getChildren()) {
					results.add(result);
				}
			}
		}
		return results;
	}

	/**
	 * Returns a sorted list (by name) of all test methods.
	 */
	public List<TestMethod> listSortedTestMethods() {
		List<TestMethod> results = listTestMethods();
		Collections.sort(results);
		return results;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * It is asserted that the parent parameter is <code>null</code> as this is
	 * the root of a test.
	 */
	@Override
	public TestRoot deepClone(TestNodeBase parent) throws DeepCloneException {
		CCSMAssert.isTrue(parent == null,
				"A TestRoot cannot have a parent node.");
		return new TestRoot(this);
	}
}