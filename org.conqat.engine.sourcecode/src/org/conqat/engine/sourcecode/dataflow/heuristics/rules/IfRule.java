/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: IfRule.java 51147 2014-11-14 10:14:06Z streitel $            
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
 * Handles transforming if-elseif-else statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: B46820FC2646C051C296CC722BBEFD7A
 */
public class IfRule implements IControlFlowRule {

	/** The matcher that identifies "else if" clauses. */
	private final IShallowEntityMatcher elseIfMatcher;

	/** The matcher that identifies else clauses. */
	private final IShallowEntityMatcher elseMatcher;

	/** Constructor. */
	public IfRule(IShallowEntityMatcher elseIfMatcher,
			IShallowEntityMatcher elseMatcher) {
		this.elseIfMatcher = elseIfMatcher;
		this.elseMatcher = elseMatcher;
	}

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ControlFlowNode ifNode = context.createNode(entities.get(0)
				.ownStartTokens(), true);
		List<ControlFlowNode> exitNodes = new ArrayList<ControlFlowNode>();
		int consumedEntities = 1;

		Result thenBranch = RuleUtils.transformInNewScope(creator, context,
				entities.get(0).getChildren());
		ControlFlowNode.link(ifNode, thenBranch.getEntryNode());
		exitNodes.addAll(thenBranch.getExitNodes());

		List<Result> elseIfBranches = parseElseIfBranches(entities, creator,
				context);
		consumedEntities += elseIfBranches.size();

		ControlFlowNode lastNode = ifNode;
		for (Result elseIfBranch : elseIfBranches) {
			ControlFlowNode.link(lastNode, elseIfBranch.getEntryNode());
			exitNodes.addAll(elseIfBranch.getExitNodes());
			lastNode = elseIfBranch.getEntryNode();
		}

		Result elseBranch = RuleUtils.parseEntityIfExists(elseMatcher,
				entities, creator, context, elseIfBranches.size() + 1);
		if (elseBranch == null) {
			exitNodes.add(lastNode);
		} else {
			consumedEntities += 1;
			ControlFlowNode.link(lastNode, elseBranch.getEntryNode());
			exitNodes.addAll(elseBranch.getExitNodes());
		}

		return new Result(consumedEntities, ifNode, exitNodes);
	}

	/**
	 * Parses all "else if" entities that follow the if entity at index 0 of the
	 * given list.
	 */
	private List<Result> parseElseIfBranches(List<ShallowEntity> entities,
			ControlFlowCreator creator, Context context) {
		List<Result> results = new ArrayList<Result>();

		int currentNode = 1;
		while (currentNode < entities.size()) {
			ShallowEntity elseIfEntity = entities.get(currentNode);
			if (!elseIfMatcher.matches(elseIfEntity)) {
				break;
			}

			Result result = RuleUtils.transformInNewScope(creator, context,
					elseIfEntity.getChildren());

			ControlFlowNode elseIfNode = context.createNode(
					elseIfEntity.ownStartTokens(), true);
			ControlFlowNode.link(elseIfNode, result.getEntryNode());
			results.add(new Result(result.getNumberOfConsumedEntities(),
					elseIfNode, result.getExitNodes()));

			currentNode++;
		}

		return results;
	}

}