/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CFGComparer.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Stack;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.utils.GraphDebuggingUtils;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.lib.commons.collections.BidirectionalMap;
import org.conqat.lib.commons.collections.ListMap;

/**
 * Compares a CFG to a "human-readable" specification.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: 218F54018CBF90451BCEEDD288A6F23F
 */
public class CFGComparer {

	/** The ID of the root node. */
	private final Integer rootId;

	/** The ids of the children each node should have. */
	private final ListMap<Integer, Integer> expectedChildren = new ListMap<Integer, Integer>();

	/** Maps from nodes to their assigned IDs. */
	private final BidirectionalMap<ControlFlowNode, Integer> idMap = new BidirectionalMap<ControlFlowNode, Integer>();

	/** Constructor. */
	public CFGComparer(Integer rootNode) {
		this.rootId = rootNode;
	}

	/** Declares the children of a node. */
	public CFGComparer node(Integer id, Integer... children) {
		for (Integer child : children) {
			expectedChildren.add(id, child);
		}
		return this;
	}

	/**
	 * Asserts that the given tree of {@link ControlFlowNode}s meets the
	 * specification set out by {@link #node(Integer, Integer...)} calls.
	 */
	public void assertMeetsSpecification(ControlFlowNode root) {
		idMap.put(root, rootId);
		Stack<Integer> path = new Stack<Integer>();
		path.push(rootId);
		assertChildren(root, rootId, path, root);
	}

	/**
	 * Recursively asserts that the given node's children match the
	 * specification.
	 */
	protected void assertChildren(ControlFlowNode node, Integer id,
			Stack<Integer> path, ControlFlowNode root) {
		List<Integer> expectedIds = expectedChildren.getCollection(id);
		if (expectedIds == null) {
			assertEquals(
					createAssertionMessage(
							"The node at path "
									+ path.toString()
									+ " does not have the expected amount of children",
							node, root), 0, node.getSuccessors().size());
			return;
		}

		assertEquals(
				createAssertionMessage("The node at path " + path.toString()
						+ " does not have the expected amount of children",
						node, root), expectedIds.size(), node.getSuccessors()
						.size());

		for (int i = 0; i < node.getSuccessors().size(); i++) {
			ControlFlowNode child = node.getSuccessors().get(i);
			Integer childId = idMap.getSecond(child);
			Integer expectedId = expectedIds.get(i);
			if (childId == null) {
				if (idMap.containsSecond(expectedId)) {
					fail(createAssertionMessage("I expected that child " + i
							+ " of the node at path " + path.toString()
							+ " were node " + expectedId
							+ " but instead I found some other node", child,
							root));
				}

				idMap.put(child, expectedId);
				path.push(expectedId);
				assertChildren(child, expectedId, path, root);
				path.pop();
			} else {
				assertEquals(
						createAssertionMessage("Expected the node at path "
								+ path.toString() + " to have node "
								+ expectedId + " as its child number " + i
								+ " but found node " + childId + " instead",
								child, root), expectedId, childId);
			}
		}
	}

	/** Creates a useful message for assertions to make debugging easier. */
	protected String createAssertionMessage(String specificError,
			ControlFlowNode errorNode, ControlFlowNode rootNode) {
		return "The CFG does not match the specification.\nInput CFG:\n"
				+ GraphDebuggingUtils
						.toString(rootNode, ControlFlowNode.WALKER)
				+ "Specific mismatch:\n" + specificError
				+ "\nTokens of the mismatched node:\n"
				+ TokenStreamUtils.toString(errorNode.getTokens()) + "\n";
	}
}
