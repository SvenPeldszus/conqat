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

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.sourcecode.test.TestResultReaderBase;
import org.conqat.engine.sourcecode.test.TestResultReaderBase.ResultIdStrategy;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.clone.DeepCloneException;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Represents the execution of a single test method.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: F92D748485309D89D35D566FB0BEF7F7
 */
// TODO (FS) why is this a container? wouldn't it be enough to extend
// TestNodeBase?
public class TestMethod extends TestNodeContainer<TestContainer, TestNodeBase>
		implements Comparable<TestMethod> {

	/** The unique id of the node. */
	private final String id;

	/**
	 * Constructor. Adds this {@link TestMethod} to the provided
	 * {@link TestContainer}
	 */
	public TestMethod(TestContainer container, String name,
			ResultIdStrategy idStrategy) {
		super(container, name, TestNodeBase.class);
		this.id = idStrategy.determineId(container.getId(), name);

		// set default values
		setExecutionTime(0);
		setAssessment(ETrafficLightColor.UNKNOWN, StringUtils.EMPTY_STRING);
	}

	/**
	 * Clone constructor with the given parent node.
	 * 
	 * @see TestNodeContainer#TestNodeContainer(TestNodeBase, TestNodeContainer)
	 */
	private TestMethod(TestContainer parent, TestMethod method)
			throws DeepCloneException {
		super(parent, method);
		this.id = method.id;
	}

	/** {@inheritDoc} */
	@Override
	public String getId() {
		return id;
	}

	/** {@inheritDoc} */
	@Override
	public TestMethod deepClone(TestNodeBase parent) throws DeepCloneException {
		return new TestMethod(CCSMAssert.checkedCast(parent,
				TestContainer.class), this);
	}

	/** {@inheritDoc} */
	@Override
	public int compareTo(TestMethod other) {
		return this.getName().compareTo(other.getName());
	}

	/**
	 * Assigns the execution result and assessment to the {@link TestMethod}.
	 * <p>
	 * Sets {@value TestResultReaderBase#ASSESSMENT_KEY} and
	 * {@value TestResultReaderBase#RESULT_KEY}.
	 */
	public void setAssessment(ETrafficLightColor color, String result) {
		setValue(TestResultReaderBase.ASSESSMENT_KEY, new Assessment(color));
		addResult(result);
	}

	/**
	 * Adds a new execution result to the {@link TestMethod}. This method is
	 * usually called by a parser after gaining more detailed information, e.g.
	 * the console output. The new result is appended to the existing one and
	 * separated by a blank line.
	 * <p>
	 * Sets {@value TestResultReaderBase#RESULT_KEY}.
	 */
	public void addResult(String text) {
		if (text == null) {
			return;
		}

		text = StringUtils.removeWhitespaceAtBeginningOfLine(text.trim());
		String result = getResult();

		if (StringUtils.isEmpty(result)) {
			result = text;
		} else if (!StringUtils.isEmpty(text)) {
			result += StringUtils.CR + StringUtils.CR + text;
		}

		setValue(TestResultReaderBase.RESULT_KEY, result);
	}

	/**
	 * Returns the textual result (outcome) of a test method or the empty string
	 * if no outcome was stored yet or is empty.
	 * <p>
	 * Gets {@value TestResultReaderBase#RESULT_KEY}.
	 */
	public String getResult() {
		return NodeUtils.getStringValue(this, TestResultReaderBase.RESULT_KEY,
				StringUtils.EMPTY_STRING);
	}

	/**
	 * Assigns the execution time of the test method in seconds.
	 * <p>
	 * Sets {@value TestResultReaderBase#EXECUTION_TIME_KEY}.
	 */
	public void setExecutionTime(double duration) {
		setValue(TestResultReaderBase.EXECUTION_TIME_KEY, duration);
	}

	/**
	 * Gets the execution time of the test method in seconds.
	 * <p>
	 * Gets {@value TestResultReaderBase#EXECUTION_TIME_KEY}.
	 */
	public double getExecutionTime() {
		return NodeUtils.getDoubleValue(this,
				TestResultReaderBase.EXECUTION_TIME_KEY, 0);
	}

	/**
	 * Gets the test assessment of the test method.
	 * <p>
	 * Gets {@value TestResultReaderBase#ASSESSMENT_KEY}.
	 */
	public Assessment getAssessment() {
		return NodeUtils.getValue(this, TestResultReaderBase.ASSESSMENT_KEY,
				Assessment.class, null);
	}
}
