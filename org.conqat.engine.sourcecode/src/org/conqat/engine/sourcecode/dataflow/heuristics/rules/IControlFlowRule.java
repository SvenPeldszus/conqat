/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: IControlFlowRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * A rule for transforming one or more {@link ShallowEntity}s into
 * {@link ControlFlowNode}s.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: E53F139E10ED90B1B399B920301B987B
 */
public interface IControlFlowRule {

	/**
	 * Transforms a range of entities at the start of the given list into a CFG
	 * structure.
	 * 
	 * @return the result of the transformation. Returning <code>null</code>
	 *         will cause the {@link ControlFlowCreator} to ignore the first
	 *         entity in the list and resume parsing after it.
	 */
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator);

	/** The result of a transformation. */
	public static class Result {

		/** The number of entities consumed by the rule. */
		private final int numberOfConsumedEntities;

		/** The node that should be connected to the predecessor node. */
		private final ControlFlowNode entryNode;

		/** The nodes that should be connected to the successor node. */
		private final List<ControlFlowNode> exitNodes;

		/** Constructor. */
		public Result(int numberOfConsumedEntities, ControlFlowNode entryNode,
				List<ControlFlowNode> exitNodes) {
			this.numberOfConsumedEntities = numberOfConsumedEntities;
			this.entryNode = entryNode;
			this.exitNodes = exitNodes;
		}

		/**
		 * Returns the number of shallow entities consumed in order to construct
		 * this result.
		 */
		public int getNumberOfConsumedEntities() {
			return numberOfConsumedEntities;
		}

		/** Returns the root node of the constructed CFG subgraph. */
		public ControlFlowNode getEntryNode() {
			return entryNode;
		}

		/** Returns the exit nodes of the constructed CFG subgraph. */
		public List<ControlFlowNode> getExitNodes() {
			return exitNodes;
		}

	}

}
