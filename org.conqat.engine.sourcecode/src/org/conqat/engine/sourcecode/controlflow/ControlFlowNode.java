/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ControlFlowNode.java 51696 2015-02-06 14:01:51Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.error.NeverThrownRuntimeException;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.visitor.IMeshWalker;
import org.conqat.lib.commons.visitor.VisitorUtils;
import org.conqat.lib.scanner.IToken;

/**
 * Models the control flow graph in a language independent manner and keeps some
 * further information about the statement it represents, which is needed for
 * the data flow algorithms.
 * 
 * These assumptions are made:
 * <ul>
 * <li>Each statement has successors, one of which is executed after it.
 * Statements with more than one successor represent branch points (e.g. if or
 * while)
 * <li>Each statement can enumerate all variables that are used in computations
 * and evaluations
 * <li>Each statement can enumerate all variables to which values are assigned
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51696 $
 * @ConQAT.Rating YELLOW Hash: A63284073D9E7DC3D11F456ACFDDCB8B
 */
public class ControlFlowNode {

	/**
	 * Used to traverse a {@link ControlFlowNode} graph with the
	 * {@link VisitorUtils}.
	 */
	public static final MeshWalker WALKER = new MeshWalker();

	/** The identifiers assigned in the statement. */
	private final VariableReadWriteInfo readWriteInfo;

	/** The successors of the statement. */
	private final List<ControlFlowNode> successors = new ArrayList<ControlFlowNode>();

	/** The predecessors of the statement. */
	private final List<ControlFlowNode> predecessors = new ArrayList<ControlFlowNode>();

	/** The tokens that belong to the node. For synthetic nodes this is null. */
	private final List<IToken> tokens;

	/** The condition, in case this is a conditional node. */
	private Condition condition = null;

	/** Whether this is an artificial assume node. */
	private final boolean isAssumeNode;

	/** Constructor for synthetic nodes. */
	public ControlFlowNode() {
		readWriteInfo = new VariableReadWriteInfo();
		tokens = null;
		isAssumeNode = false;
	}

	/** Constructor for assume nodes. */
	public ControlFlowNode(VariableReadWriteInfo assumes) {
		readWriteInfo = assumes;
		tokens = null;
		isAssumeNode = true;
	}

	/** Constructor for nodes that correspond to actual statements. */
	public ControlFlowNode(List<IToken> tokens, VariableReadWriteInfo info) {
		this.tokens = tokens;
		readWriteInfo = info;
		isAssumeNode = false;
	}

	/** Returns isConditional. */
	public boolean isConditional() {
		return condition != null;
	}

	/** @see #isAssumeNode */
	public boolean isAssumeNode() {
		return isAssumeNode;
	}

	/** Returns condition. */
	public Condition getCondition() {
		return condition;
	}

	/**
	 * Returns the node that lies at the end of the given path.
	 * 
	 * @param path
	 *            indices into the {@link #successors} lists.
	 */
	public ControlFlowNode getPath(Integer... path) {
		ControlFlowNode node = this;
		for (Integer index : path) {
			node = node.getSuccessors().get(index);
		}
		return node;
	}

	/**
	 * Makes this node a conditional. It's yes branch must be linked first, then
	 * its no branch.
	 */
	public void makeConditional(Condition condition) {
		this.condition = condition;
	}

	/** Returns the yes branch, if this is a conditional node. */
	public ControlFlowNode getYesBranch() {
		return successors.get(0);
	}

	/** Returns the no branch, if this is a conditional node. */
	public ControlFlowNode getNoBranch() {
		return successors.get(1);
	}

	/** Returns readWriteInfo. */
	public VariableReadWriteInfo getReadWriteInfo() {
		return readWriteInfo;
	}

	/** Returns successors. */
	public List<ControlFlowNode> getSuccessors() {
		return successors;
	}

	/** Returns predecessors. */
	public List<ControlFlowNode> getPredecessors() {
		return predecessors;
	}

	/** Returns isSynthetic. */
	public boolean isSynthetic() {
		return tokens == null;
	}

	/**
	 * Returns the tokens that belong to the node. Never returns
	 * <code>null</code>.
	 */
	public List<IToken> getTokens() {
		if (tokens == null) {
			return CollectionUtils.emptyList();
		}
		return tokens;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		if (isAssumeNode) {
			return "assume: "
					+ StringUtils.concat(readWriteInfo.getAllWrites(), "; ");
		}
		if (isSynthetic()) {
			return "SYNTHETIC";
		}
		return TokenStreamUtils.toString(tokens);
	}

	/**
	 * Links the given predecessor to the given successor node.
	 */
	public static void link(ControlFlowNode predecessor,
			ControlFlowNode successor) {
		predecessor.getSuccessors().add(successor);
		successor.getPredecessors().add(predecessor);
	}

	/**
	 * Replaces the given successor with the given new successor in the
	 * successor list of the given predecessor. This method takes care that the
	 * order of the outgoing branches of the predecessor is not disturbed.
	 */
	public static void replace(ControlFlowNode predecessor,
			ControlFlowNode successor, ControlFlowNode newSuccessor) {
		int index = predecessor.getSuccessors().indexOf(successor);
		CCSMAssert
				.isTrue(index != -1,
						DataflowExceptionUtils
								.createMessage(
										"Could not find the successor in the successor list of the predecessor",
										predecessor.getTokens()));

		successor.getPredecessors().remove(predecessor);
		predecessor.getSuccessors().set(index, newSuccessor);
		newSuccessor.getPredecessors().add(predecessor);
	}

	/**
	 * Inserts the given node between the given reference node and its
	 * successors.
	 */
	public static void weaveAfter(ControlFlowNode referenceNode,
			ControlFlowNode nodeToInsert) {
		for (ControlFlowNode successor : referenceNode.getSuccessors()) {
			successor.getPredecessors().remove(referenceNode);
			link(nodeToInsert, successor);
		}
		referenceNode.getSuccessors().clear();
		link(referenceNode, nodeToInsert);
	}

	/**
	 * Inserts the given node between the given reference node and its
	 * successors.
	 */
	public static void weaveBetween(ControlFlowNode node,
			ControlFlowNode successor, ControlFlowNode nodeToInsert) {
		CCSMAssert.isTrue(node.getSuccessors().contains(successor),
				"Cannot weave node between two nodes that are not linked.");
		successor.getPredecessors().remove(node);
		node.getSuccessors().remove(successor);
		link(node, nodeToInsert);
		link(nodeToInsert, successor);
	}

	/**
	 * Used to traverse a {@link ControlFlowNode} graph with the
	 * {@link VisitorUtils}.
	 */
	private static class MeshWalker implements
			IMeshWalker<ControlFlowNode, NeverThrownRuntimeException> {

		/** {@inheritDoc} */
		@Override
		public Collection<ControlFlowNode> getAdjacentElements(
				ControlFlowNode element) throws NeverThrownRuntimeException {
			return element.getSuccessors();
		}

	}

}
