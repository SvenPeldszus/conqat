/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ShallowEntityMultiMatcher.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Does case insensitive matching against a single subtype and/or name.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 902CD6F4F59D3E92F6CA9B93E6FAAE37
 */
public class ShallowEntityMultiMatcher implements IShallowEntityMatcher {

	/** The matchers, one of which must match the entity. */
	private final IShallowEntityMatcher[] matchers;

	/**
	 * Constructor.
	 * 
	 * @param matchers
	 *            a list of matchers. If at least one of them matches the
	 *            entity, this matcher matches the entity.
	 */
	public ShallowEntityMultiMatcher(IShallowEntityMatcher... matchers) {
		this.matchers = matchers;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		for (IShallowEntityMatcher matcher : matchers) {
			if (matcher.matches(entity)) {
				return true;
			}
		}
		return false;
	}
}
