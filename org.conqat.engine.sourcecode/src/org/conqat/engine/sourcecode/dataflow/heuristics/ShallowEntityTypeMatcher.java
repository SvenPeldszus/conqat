/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ShallowEntityTypeMatcher.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityTypeMatcher;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Matches an entity's type.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: B62760A847F991B11536A5D215F89496
 */
public class ShallowEntityTypeMatcher implements IShallowEntityMatcher {

	/** Matches methods. */
	public static final ShallowEntityTypeMatcher METHOD_MATCHER = new ShallowEntityTypeMatcher(
			EShallowEntityType.METHOD);

	/** The type to match. */
	private final EShallowEntityType type;

	/**
	 * Constructor.
	 */
	public ShallowEntityTypeMatcher(EShallowEntityType type) {
		this.type = type;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		return entity.getType() == type;
	}
}
