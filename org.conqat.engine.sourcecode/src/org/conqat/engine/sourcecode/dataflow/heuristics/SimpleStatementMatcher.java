/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: SimpleStatementMatcher.java 51148 2014-11-14 13:51:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Does case sensitive matching against a single name of a simple statement
 * shallow entity.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: D86E59073A83F60B1E22017D97FED2A6
 */
public class SimpleStatementMatcher implements IShallowEntityMatcher {

	/** The name to match or <code>null</code>. */
	private final String name;

	/**
	 * Constructor.
	 * 
	 * @param name
	 *            the name to match. This parameter is case sensitive.
	 */
	public SimpleStatementMatcher(String name) {
		this.name = name;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		if (!entity.getSubtype().equals(SubTypeNames.SIMPLE_STATEMENT)) {
			return false;
		}
		return name.equals(entity.getName());
	}
}
