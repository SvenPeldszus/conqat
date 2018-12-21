/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: RuleUtils.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.List;

import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.RuleUtils;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule.Result;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Contains utility methods for all rules.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 2D40CE912331EB2215534F04DB158878
 */
public class RuleUtils {

	/**
	 * Transforms the given child entities using the given creator and ensures
	 * that all variables parsed by the context's extractor are enclosed in a
	 * new scope.
	 */
	public static Result transformInNewScope(ControlFlowCreator creator,
			Context context, List<ShallowEntity> children) {
		context.getDefUseHeuristic().openNewScope();
		Result body = creator.transform(children);
		context.getDefUseHeuristic().closeCurrentScope();
		return body;
	}

	/**
	 * Parses an entity in a new scope at the given location in the entity list,
	 * if it matches the given matcher.
	 */
	public static Result parseEntityIfExists(IShallowEntityMatcher matcher,
			List<ShallowEntity> entities, ControlFlowCreator creator,
			Context context, int index) {
		if (index >= entities.size()) {
			return null;
		}

		ShallowEntity entity = entities.get(index);
		if (!matcher.matches(entity)) {
			return null;
		}

		return RuleUtils.transformInNewScope(creator, context,
				entity.getChildren());
	}

}
