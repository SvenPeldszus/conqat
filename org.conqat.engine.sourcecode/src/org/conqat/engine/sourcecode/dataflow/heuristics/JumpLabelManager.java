/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JumpLabelManager.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.ListMap;

/**
 * Manages jump labels and the nodes that jump to them.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: EF412D8A1B5C9C8676BECD52DCF31A01
 */
public class JumpLabelManager {

	/**
	 * The prefix given to jump labels that are exactly one statement after the
	 * labeled code. The "\0" ensures that these labels cannot collide with
	 * normal labels coming from the source code.
	 */
	public static final String AFTER_JUMP_LABEL_PREFIX = "after\0";

	/** The nodes which are labeled. */
	private final Map<String, ControlFlowNode> targetNodes = new HashMap<String, ControlFlowNode>();

	/** The nodes which jump to a certain label. */
	private final ListMap<String, ControlFlowNode> sourceNodes = new ListMap<String, ControlFlowNode>();

	/** Adds a new label that points to the given node. */
	public void addLabeledNode(String label, ControlFlowNode node) {
		targetNodes.put(label, node);
	}

	/** Adds a new node that jumps to the given label. */
	public void addJumpNode(ControlFlowNode node, String targetLabel) {
		sourceNodes.add(targetLabel, node);
	}

	/** Connects all nodes that refer to the same label. */
	public void connectAllNodesAndClear() {
		for (String label : sourceNodes.getKeys()) {
			List<ControlFlowNode> sources = sourceNodes.getCollection(label);
			ControlFlowNode target = targetNodes.get(label);
			CCSMAssert.isNotNull(target, DataflowExceptionUtils.createMessage(
					"Trying to jump to a label that does not exist: " + label,
					sources.get(0).getTokens()));

			for (ControlFlowNode source : sources) {
				ControlFlowNode.link(source, target);
			}
		}

		targetNodes.clear();
		sourceNodes.clear();
	}

}
