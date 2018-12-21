/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: IShallowEntityMatcher.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Used to identify whether an entity matches a certain pattern or not.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 1B6AB19ADB273BDC84F8D1E6013995FE
 */
public interface IShallowEntityMatcher {

	/** Returns <code>true</code> if the given entity matches this matcher. */
	public boolean matches(ShallowEntity entity);

	/** A matcher that matches no entity. */
	public static final IShallowEntityMatcher NULL_MATCHER = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			return false;
		}
	};

}
