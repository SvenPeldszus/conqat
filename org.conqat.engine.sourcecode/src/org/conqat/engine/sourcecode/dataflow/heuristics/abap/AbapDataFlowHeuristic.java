/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapDataFlowHeuristic.java 51706 2015-02-08 14:25:04Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.DataflowHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityMultiMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules.DoRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules.FunctionLikeRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules.LoopRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules.SimpleAndChainStatementRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.BreakRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.CaseLabelRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ContinueRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IfRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IgnoreRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ReturnRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.SwitchRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.TryCatchFinallyRule;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ELanguage;

/**
 * Data flow heuristics for the ABAP language.
 * 
 * Limitations:
 * <ul>
 * <li><code>raise</code> is treated like a return statement (i.e. a raise
 * inside a catch will incorrectly exit the current method).
 * <li>try-catch-cleanup statements are treated as if no exception occurred
 * (i.e. the catch block is ignored).
 * <li>Resumable exceptions and the <code>resume</code> keyword are ignored as
 * well.
 * <li><code>loop</code> constructs with <code>at ... endat</code> extracts will
 * cause runtime exceptions (they are obsolete, according to the SAP
 * documentation).
 * <li>preprocessor instructions are not processed, i.e. <code>define</code> and
 * <code>include</code>.
 * <li><code>catch system-exceptions</code> blocks cause runtime errors.
 * <li>Method parameters are currently not parsed, i.e. they are ignored.
 * <li><code>perform</code> statements are not inlined (i.e. data flow across
 * form boundaries is ignored). Especially variables declared in in a report and
 * accessed in a form are ignored. Forms are obsolete.
 * </ul>
 * 
 * The heuristics were constructed according to
 * http://help.sap.com/abapdocu_740/en/index.htm, Chapter ABAP Keyword
 * Documentation → ABAP Overview → ABAP Statements - Overview
 * 
 * @author $Author: streitel $
 * @version $Rev: 51706 $
 * @ConQAT.Rating YELLOW Hash: CBB8392A2AAF9720BEC115A969274512
 */
public class AbapDataFlowHeuristic extends DataflowHeuristicBase {

	/**
	 * All rules used to transform {@link ShallowEntity}s to
	 * {@link ControlFlowNode}s.
	 */
	private static final PairList<IShallowEntityMatcher, IControlFlowRule> RULES = new PairList<IShallowEntityMatcher, IControlFlowRule>();
	static {
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.TRY),
				new TryCatchFinallyRule(new AbapShallowEntityMatcher(
						SubTypeNames.CATCH), new AbapShallowEntityMatcher(
						SubTypeNames.CLEANUP)));
		RULES.add(AbapShallowEntityMatcher.RETURN_MATCHER, new ReturnRule());
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.CONTINUE),
				new ContinueRule());
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.EXIT),
				new BreakRule());
		RULES.add(ShallowEntityTypeMatcher.METHOD_MATCHER,
				new FunctionLikeRule());
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.IF), new IfRule(
				new AbapShallowEntityMatcher(SubTypeNames.ELSE_IF_NOSPACE),
				new AbapShallowEntityMatcher(SubTypeNames.ELSE)));
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.DO), new DoRule());
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.WHEN),
				new CaseLabelRule(false, new AbapShallowEntityMatcher(
						SubTypeNames.WHEN),
						AbapShallowEntityMatcher.WHEN_OTHERS_MATCHER,
						IShallowEntityMatcher.NULL_MATCHER,
						AbapShallowEntityMatcher.RETURN_MATCHER,
						SubTypeNames.WHEN, "."));
		RULES.add(new AbapShallowEntityMatcher(SubTypeNames.CASE),
				new SwitchRule());
		RULES.add(new ShallowEntityMultiMatcher(new AbapShallowEntityMatcher(
				SubTypeNames.WHILE), new AbapShallowEntityMatcher(
				SubTypeNames.LOOP), new AbapShallowEntityMatcher(
				SubTypeNames.PROVIDE), new AbapShallowEntityMatcher(
				SubTypeNames.SELECT_BLOCK)), new LoopRule());
		RULES.add(AbapShallowEntityMatcher.UNINTERESTING_ENTITIES,
				new IgnoreRule());
		// gobbles up all simple statements, thus must be the last statement
		// processing rule
		RULES.add(AbapShallowEntityMatcher.SIMPLE_STATEMENT_MATCHER,
				new SimpleAndChainStatementRule());
	}

	/** Constructor. */
	public AbapDataFlowHeuristic() {
		super(RULES, ELanguage.ABAP);
	}

}
