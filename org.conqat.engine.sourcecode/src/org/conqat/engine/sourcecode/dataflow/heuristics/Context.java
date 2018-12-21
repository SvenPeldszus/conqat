/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: Context.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Context for a {@link ControlFlowCreator} that supports break-, continue- and
 * return-like statements as well as a switch-case construct.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: 11152F65AFDFA16B3A9F67A1C147503F
 */
public class Context {

	/**
	 * The heuristic used to annotate {@link ControlFlowNode}s with def-use
	 * information.
	 */
	private final IDefUseHeuristic defUseHeuristic;

	/**
	 * The heuristic used to annotate {@link ControlFlowNode}s with condition
	 * information.
	 */
	private final ConditionHeuristicBase conditionHeuristic;

	/**
	 * Holds break statement nodes so they can be processed by the loop and
	 * switch rules. These node lists are versioned to allow several loops after
	 * each other.
	 */
	private final Stack<List<ControlFlowNode>> breakNodes = new Stack<List<ControlFlowNode>>();

	/**
	 * Holds continue statement nodes so they can be processed by the loop
	 * rules. These node lists are versioned to allow several loops after each
	 * other.
	 */
	private final Stack<List<ControlFlowNode>> continueNodes = new Stack<List<ControlFlowNode>>();

	/**
	 * Holds return and throw statement nodes so they can be processed by the
	 * method rule. These node lists are versioned to allow several scopes after
	 * each other, e.g. try-catch statements etc.
	 */
	private final Stack<List<ControlFlowNode>> returnNodes = new Stack<List<ControlFlowNode>>();

	/**
	 * Holds the entry nodes of case statements so they can be processed by the
	 * switch rule.
	 */
	private final Stack<List<ControlFlowNode>> caseEntryNodes = new Stack<List<ControlFlowNode>>();

	/**
	 * Holds the entry node of the default case statement so it can be processed
	 * by the switch rule.
	 */
	private final Stack<ControlFlowNode> defaultCaseEntryNode = new Stack<ControlFlowNode>();

	/** Manages labels given to blocks of code. */
	private final JumpLabelManager codeLabelManager = new JumpLabelManager();

	/** Manages labels given to cases in a switch statemente. */
	private final Stack<JumpLabelManager> caseLabelManager = new Stack<JumpLabelManager>();

	/** Constructor. */
	public Context(ELanguage language, IConQATLogger logger)
			throws ConQATException {
		this.defUseHeuristic = DataFlowHeuristicFactory.createDefUseHeuristic(
				language, logger);
		this.conditionHeuristic = DataFlowHeuristicFactory
				.createConditionHeuristic(language, defUseHeuristic);
		saveCurrentLoopNodes();
		saveCurrentReturnNodes();
		saveCurrentSwitchNodes();
	}

	/**
	 * Returns the def use heuristic.
	 */
	public IDefUseHeuristic getDefUseHeuristic() {
		return defUseHeuristic;
	}

	/**
	 * Creates a new {@link ControlFlowNode} for the given tokens and extracts
	 * def-use and condition information for it if necessary.
	 */
	public ControlFlowNode createNode(List<IToken> tokens,
			boolean containsCondition) {
		ControlFlowNode node = new ControlFlowNode(tokens,
				defUseHeuristic.parseStatement(tokens));
		if (containsCondition) {
			conditionHeuristic.createCondition(node);
		}
		return node;
	}

	/** Creates a new, empty synthetic {@link ControlFlowNode}. */
	public ControlFlowNode createSyntheticNode() {
		return new ControlFlowNode();
	}

	/**
	 * Creates a new {@link ControlFlowNode} with the given def-use information.
	 */
	public ControlFlowNode createNode(List<IToken> tokens,
			VariableReadWriteInfo info) {
		return new ControlFlowNode(tokens, info);
	}

	/** Returns caseLabelManager. */
	public JumpLabelManager getCaseLabelManager() {
		return caseLabelManager.peek();
	}

	/** Returns codeLabelManager. */
	public JumpLabelManager getCodeLabelManager() {
		return codeLabelManager;
	}

	/** Returns breakNodes. */
	public List<ControlFlowNode> getBreakNodes() {
		return breakNodes.peek();
	}

	/** Returns continueNodes. */
	public List<ControlFlowNode> getContinueNodes() {
		return continueNodes.peek();
	}

	/** Returns returnNodes. */
	public List<ControlFlowNode> getReturnNodes() {
		return returnNodes.peek();
	}

	/** Returns caseEntryNodes. */
	public List<ControlFlowNode> getCaseEntryNodes() {
		return caseEntryNodes.peek();
	}

	/** Returns defaultCaseEntryNode. */
	public ControlFlowNode getDefaultCaseEntryNode() {
		return defaultCaseEntryNode.peek();
	}

	/**
	 * Saves the current loop nodes (continue and break nodes) on the stack, to
	 * be restored later using {@link #restorePreviousLoopNodes()}.
	 */
	public void saveCurrentLoopNodes() {
		breakNodes.push(new ArrayList<ControlFlowNode>());
		continueNodes.push(new ArrayList<ControlFlowNode>());
	}

	/** Restores the last-saved loop nodes from the stack. */
	public void restorePreviousLoopNodes() {
		breakNodes.pop();
		continueNodes.pop();
	}

	/**
	 * Saves the current break nodes on the stack, to be restored later using
	 * {@link #restorePreviousBreakNodes()}.
	 */
	public void saveCurrentBreakNodes() {
		breakNodes.push(new ArrayList<ControlFlowNode>());
	}

	/** Restores the last-saved break nodes from the stack. */
	public void restorePreviousBreakNodes() {
		breakNodes.pop();
	}

	/**
	 * Saves the current continue nodes on the stack, to be restored later using
	 * {@link #restorePreviousContinueNodes()}.
	 */
	public void saveCurrentContinueNodes() {
		continueNodes.push(new ArrayList<ControlFlowNode>());
	}

	/** Restores the last-saved continue nodes from the stack. */
	public void restorePreviousContinueNodes() {
		continueNodes.pop();
	}

	/**
	 * Saves the current return nodes on the stack, to be restored later using
	 * {@link #restorePreviousReturnNodes()}.
	 */
	public void saveCurrentReturnNodes() {
		returnNodes.push(new ArrayList<ControlFlowNode>());
	}

	/** Restores the last-saved return nodes from the stack. */
	public void restorePreviousReturnNodes() {
		returnNodes.pop();
	}

	/**
	 * Saves the current switch nodes on the stack, to be restored later using
	 * {@link #restorePreviousSwitchNodes()}.
	 */
	public void saveCurrentSwitchNodes() {
		caseEntryNodes.push(new ArrayList<ControlFlowNode>());
		defaultCaseEntryNode.push(null);
		caseLabelManager.push(new JumpLabelManager());
	}

	/** Restores the last-saved switch nodes from the stack. */
	public void restorePreviousSwitchNodes() {
		caseEntryNodes.pop();
		defaultCaseEntryNode.pop();
		caseLabelManager.pop();
	}

	/** Sets defaultCaseEntryNode. */
	public void setDefaultCaseEntryNode(ControlFlowNode defaultCaseEntryNode) {
		this.defaultCaseEntryNode.pop();
		this.defaultCaseEntryNode.push(defaultCaseEntryNode);
	}

}
