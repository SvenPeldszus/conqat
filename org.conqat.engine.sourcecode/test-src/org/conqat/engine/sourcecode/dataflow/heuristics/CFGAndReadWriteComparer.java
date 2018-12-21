/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CFGAndReadWriteComparer.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.dataflow.heuristics.AssignmentAsserter;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGAndReadWriteComparer;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGComparer;

/**
 * An extension of the {@link CFGComparer} that also compares the
 * {@link VariableReadWriteInfo} of each {@link ControlFlowNode}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: F930463DF599FE9E6E26554E7FDCDAB7
 */
public class CFGAndReadWriteComparer extends CFGComparer {

	/** The expected definitions per node. */
	private final Map<Integer, AssignmentAsserter> definitionAsserters = new HashMap<Integer, AssignmentAsserter>();

	/** The expected assignments per node. */
	private final Map<Integer, AssignmentAsserter> assignmentAsserters = new HashMap<Integer, AssignmentAsserter>();

	/** The expected reads per node. */
	private final Map<Integer, Set<String>> expectedReads = new HashMap<Integer, Set<String>>();

	/** Constructor. */
	public CFGAndReadWriteComparer(Integer rootNode) {
		super(rootNode);
	}

	/** Declares the expected children and data flow information of a node. */
	public CFGAndReadWriteComparer node(Integer id,
			AssignmentAsserter definitionAsserter,
			AssignmentAsserter assignmentAsserter, Set<String> expectedReads,
			Integer... children) {
		this.assignmentAsserters.put(id, assignmentAsserter);
		this.definitionAsserters.put(id, definitionAsserter);
		this.expectedReads.put(id, expectedReads);
		super.node(id, children);
		return this;
	}

	/** {@inheritDoc} */
	@Override
	protected void assertChildren(ControlFlowNode node, Integer id,
			Stack<Integer> path, ControlFlowNode root) {
		AssignmentAsserter definitionAsserter = definitionAsserters.get(id);
		AssignmentAsserter assignmentAsserter = assignmentAsserters.get(id);
		Set<String> expectedReads = this.expectedReads.get(id);

		if (definitionAsserter != null) {
			definitionAsserter.assertMatches(
					node.getReadWriteInfo().getDefinitions(),
					createAssertionMessage(
							"Wrong definition information for the node at path "
									+ path, node, root));
		}
		if (assignmentAsserter != null) {
			assignmentAsserter.assertMatches(
					node.getReadWriteInfo().getAssignments(),
					createAssertionMessage(
							"Wrong assignment information for the node at path "
									+ path, node, root));
		}
		if (expectedReads != null) {
			assertEquals(
					createAssertionMessage(
							"Wrong read variables information for the node at path "
									+ path, node, root), expectedReads,
					node.getReadWriteInfo().getReads());
		}

		super.assertChildren(node, id, path, root);
	}

}
