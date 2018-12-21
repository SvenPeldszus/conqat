/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AssignmentAsserter.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.controlflow.VariableWrite;

/**
 * Checks assignments computed by the extractor.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: DAF80976F822F694AC639782162F5AC1
 */
public class AssignmentAsserter {

	/** The assignments the test expects to see. */
	private final Set<VariableWrite> expectedAssignments = new HashSet<VariableWrite>();

	/**
	 * Asserts that the given assignments equal the {@link #expectedAssignments}
	 * .
	 */
	public void assertMatches(List<VariableWrite> assignments) {
		assertMatches(assignments, "");
	}

	/**
	 * Asserts that the given assignments equal the {@link #expectedAssignments}
	 * .
	 */
	public void assertMatches(List<VariableWrite> assignments,
			String additionalMessage) {
		Set<VariableWrite> actualAssignments = new HashSet<VariableWrite>(
				assignments);
		assertEquals("The two sets do not equal:\nexpected: "
				+ expectedAssignments + "\ngot: " + actualAssignments + "\n"
				+ additionalMessage, expectedAssignments, actualAssignments);
	}

	/**
	 * Adds an expected assignment of a single value.
	 */
	public AssignmentAsserter value(String leftSideIdentifier, String value) {
		expectedAssignments.add(new VariableWrite(leftSideIdentifier)
				.setValue(value));
		return this;
	}

	/**
	 * Adds an expected assignment of a single variable.
	 */
	public AssignmentAsserter variable(String leftSideIdentifier,
			String variableName) {
		expectedAssignments.add(new VariableWrite(leftSideIdentifier)
				.setVariable(variableName));
		return this;
	}

	/**
	 * Adds an expected assignment of anything other than a single variable or
	 * value.
	 */
	public AssignmentAsserter other(String leftSideIdentifier) {
		expectedAssignments.add(new VariableWrite(leftSideIdentifier)
				.setOther());
		return this;
	}

	/**
	 * Adds an expected assignment of type "empty".
	 */
	public AssignmentAsserter empty(String leftSideIdentifier) {
		expectedAssignments.add(new VariableWrite(leftSideIdentifier)
				.setEmpty());
		return this;
	}

	/**
	 * Adds an expected assignment of type "null".
	 */
	public AssignmentAsserter nullValue(String leftSideIdentifier) {
		expectedAssignments
				.add(new VariableWrite(leftSideIdentifier).setNull());
		return this;
	}

	/**
	 * Adds an expected assignment of type "other" which is a default init.
	 */
	public AssignmentAsserter otherDefaultInit(String leftSideIdentifier) {
		expectedAssignments.add(new VariableWrite(leftSideIdentifier)
				.makeDefaultInitialization());
		return this;
	}
}