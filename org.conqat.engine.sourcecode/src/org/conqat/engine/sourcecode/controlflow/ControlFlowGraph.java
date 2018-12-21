/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ControlFlowGraph.java 51696 2015-02-06 14:01:51Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.dataflow.utils.GraphDebuggingUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.visitor.VisitorUtils;

/**
 * 
 * @author $Author: streitel $
 * @version $Rev: 51696 $
 * @ConQAT.Rating YELLOW Hash: F92201603DCDE49150EEB0325ACDA8DB
 */
public class ControlFlowGraph {

	/** The root node of the graph. */
	private final ControlFlowNode root;

	/**
	 * The name of the method from which the graph was created. May be
	 * <code>null</code>.
	 */
	private final String methodName;

	/** The entities from which the CFG was constructed. */
	private final List<ShallowEntity> entities;

	/** The exit node of the graph. */
	private final ControlFlowNode exitNode;

	/** Lazily-initialized list of all defined variables in the graph. */
	private Set<String> variables = null;

	/** Constructor. */
	public ControlFlowGraph(ControlFlowNode root, ControlFlowNode exitNode,
			String methodName, List<ShallowEntity> entities) {
		this.root = root;
		this.exitNode = exitNode;
		this.methodName = methodName;
		this.entities = entities;
	}

	/** @see #entities */
	public List<ShallowEntity> getEntities() {
		return entities;
	}

	/** @see #root */
	public ControlFlowNode getRoot() {
		return root;
	}

	/** @see #methodName */
	public String getMethodName() {
		if (methodName == null) {
			return StringUtils.EMPTY_STRING;
		}
		return methodName;
	}

	/** @see #exitNode */
	public ControlFlowNode getExitNode() {
		return exitNode;
	}

	/** Returns all nodes in this graph in depth first order. */
	public List<ControlFlowNode> listDepthFirst() {
		return VisitorUtils.listAllDepthFirst(root, ControlFlowNode.WALKER);
	}

	/** @see #variables */
	public Set<String> getVariables() {
		if (variables == null) {
			variables = new HashSet<>();
			for (ControlFlowNode node : listDepthFirst()) {
				for (VariableWrite write : node.getReadWriteInfo()
						.getAllWrites()) {
					variables.add(write.getChangedVariable());
				}
			}
		}
		return variables;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "CFG for " + methodName + ":\n"
				+ GraphDebuggingUtils.toString(root, ControlFlowNode.WALKER);
	}

}
