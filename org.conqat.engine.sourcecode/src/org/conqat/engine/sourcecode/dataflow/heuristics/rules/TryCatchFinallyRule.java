/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TryCatchFinallyRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.RuleUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Transforms try-catch-finally statements.
 * 
 * The transformation is incomplete. This rule simply creates a branch at the
 * location of the <code>try</code> statement that either goes through the
 * <code>try</code> control flow or to the <code>catch</code> blocks. The
 * <code>finally</code> block is appended to all <code>try</code> and
 * <code>catch</code> blocks and linked to the appropriate exit nodes.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 4EF9F2E629D6605ADAAA7F134D8E63C4
 */
public class TryCatchFinallyRule implements IControlFlowRule {

	/** Matches catch blocks. */
	private final IShallowEntityMatcher catchMatcher;

	/** Matches finally blocks. */
	private final IShallowEntityMatcher finallyMatcher;

	/** Constructor. */
	public TryCatchFinallyRule(IShallowEntityMatcher catchMatcher,
			IShallowEntityMatcher finallyMatcher) {
		this.catchMatcher = catchMatcher;
		this.finallyMatcher = finallyMatcher;
	}

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		int consumedEntities = 1;
		ShallowEntity tryEntity = entities.get(0);
		ControlFlowNode tryNode = context.createNode(
				tryEntity.ownStartTokens(), false);

		context.saveCurrentReturnNodes();
		context.saveCurrentLoopNodes();

		Result tryBranch = RuleUtils.transformInNewScope(creator, context,
				tryEntity.getChildren());
		List<ControlFlowNode> exitNodes = new ArrayList<ControlFlowNode>();
		exitNodes.addAll(tryBranch.getExitNodes());
		ControlFlowNode.link(tryNode, tryBranch.getEntryNode());

		List<Result> catchBranches = parseCatchBranches(entities, context,
				creator);
		consumedEntities += catchBranches.size();
		for (Result result : catchBranches) {
			ControlFlowNode.link(tryNode, result.getEntryNode());
			exitNodes.addAll(result.getExitNodes());
		}

		List<ControlFlowNode> returnNodes = context.getReturnNodes();
		context.restorePreviousReturnNodes();

		List<ControlFlowNode> continueNodes = context.getContinueNodes();
		List<ControlFlowNode> breakNodes = context.getBreakNodes();
		context.restorePreviousLoopNodes();

		Result finallyBranch = RuleUtils.parseEntityIfExists(finallyMatcher,
				entities, creator, context, catchBranches.size() + 1);
		if (finallyBranch == null) {
			context.getReturnNodes().addAll(returnNodes);
			context.getContinueNodes().addAll(continueNodes);
			context.getBreakNodes().addAll(breakNodes);
		} else {
			consumedEntities += 1;
			// link the finally branch to return, continue and break nodes
			for (ControlFlowNode node : returnNodes) {
				ControlFlowNode.link(node, finallyBranch.getEntryNode());
			}
			if (!returnNodes.isEmpty()) {
				context.getReturnNodes().addAll(finallyBranch.getExitNodes());
			}

			for (ControlFlowNode node : continueNodes) {
				ControlFlowNode.link(node, finallyBranch.getEntryNode());
			}
			if (!continueNodes.isEmpty()) {
				context.getContinueNodes().addAll(finallyBranch.getExitNodes());
			}

			for (ControlFlowNode node : breakNodes) {
				ControlFlowNode.link(node, finallyBranch.getEntryNode());
			}
			if (!breakNodes.isEmpty()) {
				context.getBreakNodes().addAll(finallyBranch.getExitNodes());
			}

			// link the finally branch to the normal exit nodes of try and catch
			for (ControlFlowNode exitNode : tryBranch.getExitNodes()) {
				ControlFlowNode.link(exitNode, finallyBranch.getEntryNode());
			}
			for (Result catchBranch : catchBranches) {
				for (ControlFlowNode exitNode : catchBranch.getExitNodes()) {
					ControlFlowNode
							.link(exitNode, finallyBranch.getEntryNode());
				}
			}

			if (catchBranches.isEmpty()) {
				// in case of an exception, the try would not be executed fully,
				// so we go directly to the finally branch to simulate that
				// possiblity
				ControlFlowNode.link(tryNode, finallyBranch.getEntryNode());
			}

			// the finally branch exit nodes are only normal exit nodes if at
			// least one try/catch branch has normal exit nodes
			if (exitNodes.size() > 0) {
				exitNodes = finallyBranch.getExitNodes();
			} else {
				exitNodes.clear();
			}
		}

		return new Result(consumedEntities, tryNode, exitNodes);
	}

	/**
	 * Returns the number of catch statements following the try statement in the
	 * given entity list.
	 */
	private List<Result> parseCatchBranches(List<ShallowEntity> entities,
			Context context, ControlFlowCreator creator) {
		int currentNode = 1;
		List<Result> results = new ArrayList<Result>();
		while (currentNode < entities.size()) {
			ShallowEntity catchEntity = entities.get(currentNode);
			if (!catchMatcher.matches(catchEntity)) {
				break;
			}

			ControlFlowNode catchNode = context.createNode(
					catchEntity.ownStartTokens(), false);
			Result result = RuleUtils.transformInNewScope(creator, context,
					catchEntity.getChildren());
			ControlFlowNode.link(catchNode, result.getEntryNode());
			results.add(new Result(result.getNumberOfConsumedEntities(),
					catchNode, result.getExitNodes()));
			currentNode++;
		}

		return results;
	}

}