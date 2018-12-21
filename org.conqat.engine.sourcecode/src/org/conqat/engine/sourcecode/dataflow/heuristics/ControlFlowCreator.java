/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ControlFlowCreator.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule.Result;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.PairList;

/**
 * Framework for transforming a list of {@link ShallowEntity}s to a
 * {@link ControlFlowNode} structure using {@link IControlFlowRule}s.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: F1D63FCCE6D5CCFD001B3DE1B71F4FFE
 */
public class ControlFlowCreator {

	/** The rules to transform the entities to a CFG. */
	private final PairList<IShallowEntityMatcher, IControlFlowRule> rules;

	/** The context to pass to all rules. */
	private final Context context;

	/**
	 * This field is <code>true</code> as long as no entity has been transformed
	 * yet.
	 */
	private boolean isFirstNonIgnoredEntity = true;

	/** Constructor. */
	public ControlFlowCreator(Context context,
			PairList<IShallowEntityMatcher, IControlFlowRule> rules) {
		this.context = context;
		this.rules = rules;
	}

	/**
	 * Transforms the given node list and returns the result. May also be used
	 * for recursive call to transform children of a {@link ShallowEntity} from
	 * within an {@link IControlFlowRule}.
	 */
	public Result transform(List<ShallowEntity> entities) {
		ControlFlowNode firstNode = null;
		Result lastResult = null;

		int currentEntity = 0;
		while (currentEntity < entities.size()) {
			List<ShallowEntity> availableEntities = entities.subList(
					currentEntity, entities.size());
			if (!isFirstNonIgnoredEntity
					&& !availableEntities.isEmpty()
					&& availableEntities.get(0).getType() == EShallowEntityType.METHOD) {
				// ignore methods inside other methods
				currentEntity += 1;
				continue;
			}

			Result result = transformOneStep(availableEntities);
			if (result == null) {
				// ignore this particular entity
				currentEntity += 1;
				continue;
			}

			isFirstNonIgnoredEntity = false;
			CCSMAssert.isTrue(result.getNumberOfConsumedEntities() > 0,
					"A control flow rule must consume at least one entity");
			if (lastResult != null) {
				connectNodes(result, lastResult);
			}

			lastResult = result;
			if (firstNode == null) {
				firstNode = result.getEntryNode();
			}
			currentEntity += result.getNumberOfConsumedEntities();
		}

		if (firstNode == null || lastResult == null) {
			// create a synthetic dummy node to make the transformation easier
			// this way, we don't need to check for null on the results of this
			// function
			ControlFlowNode node = context.createSyntheticNode();
			return new Result(0, node, Arrays.asList(node));
		}

		return new Result(entities.size(), firstNode, lastResult.getExitNodes());
	}

	/**
	 * Applies exactly one rule to the given entities and returns the result.
	 * The caller must take care to maintain state properly and consume an
	 * appropriate amount of entities.
	 */
	public Result transformOneStep(List<ShallowEntity> availableEntities) {
		IControlFlowRule rule = findApplicableRule(availableEntities);
		return rule.transform(availableEntities, context, this);
	}

	/**
	 * Properly connects the current result to its predecessor.
	 */
	private void connectNodes(Result result, Result lastResult) {
		for (ControlFlowNode predecessorNode : lastResult.getExitNodes()) {
			ControlFlowNode.link(predecessorNode, result.getEntryNode());
		}
	}

	/**
	 * Returns the first rule that applies to the given list of entities or
	 * throws an exception if none applies.
	 */
	private IControlFlowRule findApplicableRule(
			List<ShallowEntity> availableEntities) {
		for (int i = 0; i < rules.size(); i++) {
			if (rules.getFirst(i).matches(availableEntities.get(0))) {
				return rules.getSecond(i);
			}
		}

		CCSMAssert.fail(DataflowExceptionUtils.createMessage(
				"Could not find any rule that applies to the following entity list:\n"
						+ availableEntities.toString(),
				availableEntities.get(0)));
		return null;
	}

}
