/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: SubTypeMatcher.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Does case insensitive matching against a single subtype.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 0D0AFB5F6353C8AC2019A40D9A08689E
 */
public class SubTypeMatcher implements IShallowEntityMatcher {

	/** The subtype to match or <code>null</code>. */
	private final String subtype;

	/**
	 * Constructor.
	 * 
	 * @param subtype
	 *            the subtype to match. This parameter is case insensitive.
	 */
	public SubTypeMatcher(String subtype) {
		this.subtype = subtype;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		return subtype.equalsIgnoreCase(entity.getSubtype());
	}
}
