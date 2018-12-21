/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsDataFlowHeuristic.java 51706 2015-02-08 14:25:04Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import java.util.Collections;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.DataflowHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.ExitStatementEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityMultiMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.SimpleStatementMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.SubTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules.GotoRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules.MethodRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules.UsingRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules.YieldRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.AnonymousBlockRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ForRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ReturnRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.SimpleStatementRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.TryCatchFinallyRule;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;

/**
 * Data flow heuristics for the C# language.
 * 
 * What this heuristic cannot do:
 * <ul>
 * <li><code>goto</code> of any kind and <code>break</code> to label are not
 * supported but could be implemented easily.
 * <li>Exception handling control flow is not accurately represented. See
 * {@link TryCatchFinallyRule}.
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51706 $
 * @ConQAT.Rating YELLOW Hash: 6250812E39781E1B4470728B6C0CF4E7
 */
public class CsDataFlowHeuristic extends DataflowHeuristicBase {

	/**
	 * All rules used to transform {@link ShallowEntity}s to
	 * {@link ControlFlowNode}s.
	 */
	private static final PairList<IShallowEntityMatcher, IControlFlowRule> RULES = new PairList<IShallowEntityMatcher, IControlFlowRule>();
	static {
		RULES.add(
				new ShallowEntityMultiMatcher(new SubTypeMatcher(
						SubTypeNames.CHECKED), new SubTypeMatcher(
						SubTypeNames.UNCHECKED), new SubTypeMatcher(
						SubTypeNames.LOCK)), new AnonymousBlockRule());
		RULES.add(new ShallowEntityMultiMatcher(CLikeDataFlowHeuristic.FOR,
				new SubTypeMatcher(SubTypeNames.FOREACH)), new ForRule(
				ETokenType.IN));
		RULES.add(new ExitStatementEntityMatcher("Environment.Exit"),
				new ReturnRule());
		RULES.add(new SubTypeMatcher(SubTypeNames.USING), new UsingRule());
		RULES.add(new SubTypeMatcher(SubTypeNames.FIXED), new UsingRule());
		RULES.add(new SimpleStatementMatcher(SubTypeNames.YIELD),
				new YieldRule());
		RULES.add(new SimpleStatementMatcher(SubTypeNames.GOTO), new GotoRule());
		RULES.add(new SubTypeMatcher(SubTypeNames.LAMBDA_EXPRESSION),
				new SimpleStatementRule());
		RULES.add(ShallowEntityTypeMatcher.METHOD_MATCHER, new MethodRule());
		CLikeDataFlowHeuristic.addDefaultRules(RULES);
	}

	/** Constructor. */
	public CsDataFlowHeuristic() {
		super(RULES, ELanguage.CS);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Filters abstract and delegate methods.
	 */
	@Override
	public PairList<String, List<ShallowEntity>> extractExecutables(
			List<ShallowEntity> entities) {
		PairList<String, List<ShallowEntity>> executables = new PairList<String, List<ShallowEntity>>();
		List<ShallowEntity> methods = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.METHOD);
		for (ShallowEntity method : methods) {
			if (!method.getSubtype().contains("abstract")
					&& !method.getSubtype().contains("delegate")) {
				executables.add(method.getName(),
						Collections.singletonList(method));
			}
		}
		return executables;
	}

}
