/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.Collections;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.controlflow.ControlFlowGraph;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule.Result;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ELanguage;

/**
 * Base class for data flow heuristics.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: C03B5F5BDBCFD2356164DFC09A12DFD5
 */
public abstract class DataflowHeuristicBase implements IDataFlowHeuristic {

	/** The control flow construction rules. */
	private final PairList<IShallowEntityMatcher, IControlFlowRule> rules;

	/** The analyzed language. */
	private final ELanguage language;

	/**
	 * Constructor.
	 */
	public DataflowHeuristicBase(
			PairList<IShallowEntityMatcher, IControlFlowRule> rules,
			ELanguage language) {
		this.rules = rules;
		this.language = language;
	}

	/** {@inheritDoc} */
	@Override
	public ControlFlowGraph createControlFlow(List<ShallowEntity> entities,
			String name, IConQATLogger logger) throws ConQATException {
		Context context = new Context(language, logger);
		ControlFlowCreator creator = new ControlFlowCreator(context, rules);
		Result result = creator.transform(entities);

		ControlFlowNode exitNode;
		if (result.getExitNodes().size() == 1) {
			exitNode = CollectionUtils.getAny(result.getExitNodes());
		} else {
			exitNode = context.createSyntheticNode();
			for (ControlFlowNode node : result.getExitNodes()) {
				ControlFlowNode.link(node, exitNode);
			}
		}
		ControlFlowGraph graph = new ControlFlowGraph(result.getEntryNode(),
				exitNode, name, entities);
		createAssumeNodes(graph);
		return graph;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * This default implementation simply returns one executable for each method
	 * entity.
	 */
	@Override
	public PairList<String, List<ShallowEntity>> extractExecutables(
			List<ShallowEntity> entities) {
		PairList<String, List<ShallowEntity>> executables = new PairList<String, List<ShallowEntity>>();
		List<ShallowEntity> methods = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.METHOD);
		for (ShallowEntity method : methods) {
			executables
					.add(method.getName(), Collections.singletonList(method));
		}
		return executables;
	}

	/** Creates all assume nodes. */
	private static void createAssumeNodes(ControlFlowGraph graph) {
		for (ControlFlowNode node : graph.listDepthFirst()) {
			Condition condition = node.getCondition();
			if (condition == null || node.getSuccessors().size() < 2) {
				continue;
			}

			// need to store a reference to these nodes here since they are
			// re-linked with the next method call
			ControlFlowNode yesBranch = node.getYesBranch();
			ControlFlowNode noBranch = node.getNoBranch();

			createAssumeNode(node, yesBranch, condition.getYesBranchInfo());
			createAssumeNode(node, noBranch, condition.getNoBranchInfo());
		}
	}

	/**
	 * Creates the assume node for the given branch of the given node.
	 */
	private static void createAssumeNode(ControlFlowNode node,
			ControlFlowNode branch, PairList<String, Boolean> branchInfo) {
		if (branchInfo.size() == 0) {
			return;
		}

		VariableReadWriteInfo info = new VariableReadWriteInfo();
		for (int i = 0; i < branchInfo.size(); i++) {
			VariableWrite write = new VariableWrite(branchInfo.getFirst(i));
			if (branchInfo.getSecond(i)) {
				write.setNull();
			} else {
				write.setValue("assume non-null");
			}
			info.getAssignments().add(write);
		}

		ControlFlowNode assumeNode = new ControlFlowNode(info);
		ControlFlowNode.weaveBetween(node, branch, assumeNode);
	}

}
