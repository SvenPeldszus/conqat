/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapRuleUtils.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules;

import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Contains helper functions for ABAP rules.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: BA03646D29B688BC787C2DA110F53000
 */
public abstract class AbapRuleUtils implements IControlFlowRule {

	/**
	 * Returns <code>true</code> if the given entity has one of the given
	 * subtypes (ignoring casing) or if it has the subtype "subtype:" (i.e.
	 * chain statement).
	 */
	public static boolean hasSubtype(ShallowEntity entity, String... subtypes) {
		for (String subtype : subtypes) {
			if (entity.getSubtype().equalsIgnoreCase(subtype)
					|| entity.getSubtype().equalsIgnoreCase(subtype + ":")) {
				return true;
			}
		}
		return false;
	}

}