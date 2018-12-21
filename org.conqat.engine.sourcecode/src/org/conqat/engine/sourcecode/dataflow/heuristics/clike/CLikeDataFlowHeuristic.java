/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CLikeDataFlowHeuristic.java 51148 2014-11-14 13:51:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.clike;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityMultiMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.SimpleStatementMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.SubTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.AnonymousBlockRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.BreakRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.CLikeWhileRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.CaseLabelRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.CodeLabelRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ContinueRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.DoWhileRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IfRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IgnoreRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ReturnRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.SimpleStatementRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.SwitchRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.TryCatchFinallyRule;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.PairList;

/**
 * Collects all {@link IShallowEntityMatcher} for C-like shallow parsed
 * languages as well as some default {@link IControlFlowRule}s that apply to all
 * C-like languages.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: FB368249175BA75E5B98679070671C51
 */
public class CLikeDataFlowHeuristic {

	/** Matches break statements. */
	public static final IShallowEntityMatcher BREAK = new SimpleStatementMatcher(
			"break");

	/** Matches continue statements. */
	public static final IShallowEntityMatcher CONTINUE = new SimpleStatementMatcher(
			SubTypeNames.CONTINUE);

	/** Matches labels that label statements. */
	public static final IShallowEntityMatcher CODE_LABEL = new SubTypeMatcher(
			SubTypeNames.LABEL);

	/** Matches for statements. */
	public static final IShallowEntityMatcher FOR = new SubTypeMatcher(
			SubTypeNames.FOR);

	/** Matches do...while loops. */
	public static final IShallowEntityMatcher DO_LOOP = new SubTypeMatcher(
			SubTypeNames.DO);

	/** Matches while loops. */
	public static final IShallowEntityMatcher WHILE = new SubTypeMatcher(
			SubTypeNames.WHILE);

	/** Matches switch statements. */
	public static final IShallowEntityMatcher SWITCH = new SubTypeMatcher(
			SubTypeNames.SWITCH);

	/** Matches try statements. */
	public static final IShallowEntityMatcher TRY = new SubTypeMatcher(
			SubTypeNames.TRY);

	/** Matches catch clauses. */
	public static final IShallowEntityMatcher CATCH = new SubTypeMatcher(
			SubTypeNames.CATCH);

	/** Matches finally clauses. */
	public static final IShallowEntityMatcher FINALLY = new SubTypeMatcher(
			SubTypeNames.FINALLY);

	/** Matches if statements. */
	public static final IShallowEntityMatcher IF = new SubTypeMatcher(
			SubTypeNames.IF);

	/** Matches else if clauses. */
	public static final IShallowEntityMatcher ELSE_IF = new SubTypeMatcher(
			SubTypeNames.ELSE_IF);

	/** Matches else clauses. */
	public static final IShallowEntityMatcher ELSE = new SubTypeMatcher(
			SubTypeNames.ELSE);

	/** Matches case labels, excluding the default case. */
	public static final IShallowEntityMatcher CASE = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			String subtype = entity.getSubtype();
			return subtype != null && subtype.startsWith("case");
		}
	};

	/** Matches the default case label. */
	public static final IShallowEntityMatcher DEFAULT = new SubTypeMatcher(
			SubTypeNames.DEFAULT);

	/** Matches all case labels, including the default case. */
	public static final IShallowEntityMatcher ALL_CASE_LABELS = new ShallowEntityMultiMatcher(
			CASE, DEFAULT);

	/** Matches anonymous blocks clauses. */
	public static final IShallowEntityMatcher ANONYMOUS_BLOCK = new SubTypeMatcher(
			SubTypeNames.ANONYMOUS_BLOCK);

	/** Matches all simple statements. */
	public static final IShallowEntityMatcher SIMPLE_STATEMENTS = new ShallowEntityMultiMatcher(
			new SubTypeMatcher(SubTypeNames.SIMPLE_STATEMENT),
			new SubTypeMatcher(SubTypeNames.EMPTY_STATEMENT),
			new SubTypeMatcher(SubTypeNames.LOCAL_VARIABLE));

	/** Matches return and throw statements. */
	public static final IShallowEntityMatcher RETURN_OR_THROW_STATEMENTS = new ShallowEntityMultiMatcher(
			new SimpleStatementMatcher("return"), new SimpleStatementMatcher(
					"throw"));

	/**
	 * Matches entities that do not represent control flow, e.g. annotations.
	 * NOTE that this includes some of the other matchers defined in this class
	 * and should only be used as a "last-resort" matcher or default case.
	 */
	public static final IShallowEntityMatcher UNINTERESTING_ENTITIES = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			return entity.getType() == EShallowEntityType.META
					|| entity.getType() == EShallowEntityType.TYPE;
		}
	};

	/**
	 * Adds the default rules that apply to all C-like languages. Other rules
	 * that are specific to one language should be added to the rule list before
	 * calling this function.
	 */
	public static void addDefaultRules(
			PairList<IShallowEntityMatcher, IControlFlowRule> rules) {
		rules.add(ANONYMOUS_BLOCK, new AnonymousBlockRule());
		rules.add(ALL_CASE_LABELS, new CaseLabelRule(true, CASE, DEFAULT,
				BREAK, RETURN_OR_THROW_STATEMENTS, SubTypeNames.CASE, ":"));
		rules.add(SWITCH, new SwitchRule());
		rules.add(RETURN_OR_THROW_STATEMENTS, new ReturnRule());
		rules.add(CONTINUE, new ContinueRule());
		rules.add(BREAK, new BreakRule());
		rules.add(DO_LOOP, new DoWhileRule());
		rules.add(WHILE, new CLikeWhileRule());
		rules.add(TRY, new TryCatchFinallyRule(CATCH, FINALLY));
		rules.add(IF, new IfRule(ELSE_IF, ELSE));
		rules.add(CODE_LABEL, new CodeLabelRule());
		// the statement rule must be last since it gobbles up all simple
		// statements, including breaks, returns etc.
		rules.add(SIMPLE_STATEMENTS, new SimpleStatementRule());
		// causes unwanted entities to be ignored
		rules.add(UNINTERESTING_ENTITIES, new IgnoreRule());
	}

}
