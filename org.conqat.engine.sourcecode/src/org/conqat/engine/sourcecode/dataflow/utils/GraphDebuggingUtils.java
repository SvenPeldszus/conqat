/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: GraphDebuggingUtils.java 51535 2015-01-19 07:52:55Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils;

import java.util.ArrayList;
import java.util.List;

import org.conqat.lib.commons.collections.IdManager;
import org.conqat.lib.commons.error.NeverThrownRuntimeException;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.visitor.IMeshWalker;
import org.conqat.lib.commons.visitor.VisitorUtils;

/**
 * Useful methods for debugging arbitrary graphs.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51535 $
 * @ConQAT.Rating YELLOW Hash: 14D82FE2A65AA9784904DAF319C667E2
 */
public class GraphDebuggingUtils {

	/**
	 * Returns a string representation of the graph. The individual nodes are
	 * represented using their toString() method. This is useful for debugging.
	 */
	public static <NodeT> String toString(NodeT root,
			IMeshWalker<NodeT, NeverThrownRuntimeException> walker) {
		StringBuilder sb = new StringBuilder();
		IdManager<NodeT> idManager = new IdManager<NodeT>();
		List<NodeT> nodes = VisitorUtils.listAllDepthFirst(root, walker);
		for (NodeT node : nodes) {
			sb.append(idManager.obtainId(node));
			sb.append(": { ");
			sb.append(node.toString());
			sb.append(" } --> ");
			List<Integer> childIds = new ArrayList<Integer>();
			for (NodeT child : walker.getAdjacentElements(node)) {
				childIds.add(idManager.obtainId(child));
			}
			sb.append(StringUtils.concat(childIds, ", "));
			sb.append("\n");
		}
		return sb.toString();
	}

	/**
	 * Returns a human-readable string representation of the graph. The
	 * individual nodes are only represented as integers. This is useful for
	 * debugging.
	 */
	public static <NodeT> String toShortString(NodeT root,
			IMeshWalker<NodeT, NeverThrownRuntimeException> walker) {
		StringBuilder sb = new StringBuilder();
		IdManager<NodeT> idManager = new IdManager<NodeT>();

		List<NodeT> nodes = VisitorUtils.listAllDepthFirst(root, walker);
		for (NodeT node : nodes) {
			sb.append(idManager.obtainId(node));
			sb.append(" --> ");
			List<Integer> childIds = new ArrayList<Integer>();
			for (NodeT child : walker.getAdjacentElements(node)) {
				childIds.add(idManager.obtainId(child));
			}
			sb.append(StringUtils.concat(childIds, ", "));
			sb.append("\n");
		}

		return sb.toString();
	}

}
