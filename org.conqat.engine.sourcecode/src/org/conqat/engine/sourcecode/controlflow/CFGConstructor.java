/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CFGConstructor.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.analysis.ElementAnalyzerBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.DataFlowHeuristicFactory;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDataFlowHeuristic;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.PairList;

/**
 * {@ConQATDoc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: 5ED7AFCDB26EF81E5E6AB749A1DEA9C5
 */
@AConQATProcessor(description = ""
		+ "Creates a CFG for every function-like entity within the given resources and "
		+ "attaches the CFGs to the element they belong to.")
public class CFGConstructor extends
		ElementAnalyzerBase<ITokenResource, ITokenElement> {

	/** {@ConQATDoc} */
	@AConQATKey(type = "List<ControlFlowGraph>", description = "The key under which the CFGs are stored.")
	public static final String CFG_KEY = "CFGs";

	/** {@ConQATDoc} */
	@AConQATKey(type = "List<ShallowEntity>", description = "The key under which the shallow entities are stored.")
	public static final String ENTITIES_KEY = "ShallowEntities";

	/**
	 * {@inheritDoc}
	 * 
	 * @throws ConQATException
	 *             if shallow parsing the element or creating a heuristic for it
	 *             fails
	 */
	@Override
	protected void analyzeElement(ITokenElement element) throws ConQATException {
		if (!ShallowParserFactory.supportsLanguage(element.getLanguage())
				|| !DataFlowHeuristicFactory.supportsLanguage(element
						.getLanguage())) {
			return;
		}

		List<ShallowEntity> entities = ShallowParserFactory.parse(element,
				getLogger());
		IDataFlowHeuristic heuristic = DataFlowHeuristicFactory
				.createDataFlowHeuristic(element.getLanguage());
		PairList<String, List<ShallowEntity>> executables = heuristic
				.extractExecutables(entities);

		List<ControlFlowGraph> graphs = new ArrayList<ControlFlowGraph>();
		for (int i = 0; i < executables.size(); i++) {
			List<ShallowEntity> executableEntities = executables.getSecond(i);
			String executableName = executables.getFirst(i);

			try {
				ControlFlowGraph graph = heuristic.createControlFlow(
						executableEntities, executableName, getLogger());
				graphs.add(graph);
			} catch (ConQATException | AssertionError e) {
				getLogger().error(
						"An error occurred while trying to construct a CFG for function '"
								+ executableName + "' in element "
								+ element.getUniformPath()
								+ ". This function will be ignored.", e);
			}
		}

		element.setValue(CFG_KEY, graphs);
		element.setValue(ENTITIES_KEY, entities);
	}

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] {};
	}

}
