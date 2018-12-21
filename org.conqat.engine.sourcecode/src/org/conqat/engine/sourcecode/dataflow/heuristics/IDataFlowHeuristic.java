/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: IDataFlowHeuristic.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.sourcecode.controlflow.ControlFlowGraph;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.PairList;

/**
 * A language specific heuristic for creating the dataflow data structures.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: 53F4B6F27C556ED72EEC824467CCA22E
 */
public interface IDataFlowHeuristic {

	/**
	 * Creates the control flow of the body of the given method entity. Returns
	 * a pair of (root node, exit node). Must never return <code>null</code> and
	 * both nodes must never be <code>null</code>.
	 */
	public ControlFlowGraph createControlFlow(List<ShallowEntity> entities,
			String name, IConQATLogger logger) throws ConQATException;

	/**
	 * Returns all entity lists that are deemed to represent executable code.
	 * 
	 * @return a list of pairs (name, entities), each of which represents one
	 *         executable method, function or the like.
	 */
	public PairList<String, List<ShallowEntity>> extractExecutables(
			List<ShallowEntity> entities);

}
