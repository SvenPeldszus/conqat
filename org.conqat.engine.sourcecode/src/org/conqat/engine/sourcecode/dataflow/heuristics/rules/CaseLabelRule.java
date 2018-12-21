/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CaseLabelRule.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityMultiMatcher;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms case labels, including the default case label.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: DB90A732E936F1AD1C71DAC032228ECA
 */
public class CaseLabelRule implements IControlFlowRule {

	/** Whether case fall-through is possible in the language. */
	private final boolean canFallThrough;

	/** Matches the default case label. */
	private final IShallowEntityMatcher defaultMatcher;

	/** Matches break statements. */
	private final IShallowEntityMatcher breakMatcher;

	/** Matches return statements. */
	private final IShallowEntityMatcher returnMatcher;

	/** Matches both normal and the default case label. */
	private final ShallowEntityMultiMatcher allCaseLabelsMatcher;

	/** The prefix to strip from case labels */
	private final String caseLabelPrefix;

	/** The suffix to strip from case labels */
	private final String caseLabelSuffix;

	/**
	 * Constructor.
	 * 
	 * @param canFallThrough
	 *            whether the language allows case fall-through.
	 * @param caseMatcher
	 *            a matcher that matches case labels (not default labels).
	 * @param defaultMatcher
	 *            a mather that matches only default labels.
	 * @param breakMatcher
	 *            a matcher that matches break nodes.
	 * @param returnMatcher
	 *            a matcher that matches return nodes.
	 * @param caseLabelPrefix
	 *            a prefix to strip from case labels (e.g. "case" or "when").
	 * @param caseLabelSuffix
	 *            a suffix to strip from case labels (e.g. "case" or "when").
	 */
	public CaseLabelRule(boolean canFallThrough,
			IShallowEntityMatcher caseMatcher,
			IShallowEntityMatcher defaultMatcher,
			IShallowEntityMatcher breakMatcher,
			IShallowEntityMatcher returnMatcher, String caseLabelPrefix,
			String caseLabelSuffix) {
		this.caseLabelPrefix = caseLabelPrefix;
		this.caseLabelSuffix = caseLabelSuffix;
		this.allCaseLabelsMatcher = new ShallowEntityMultiMatcher(caseMatcher,
				defaultMatcher);
		this.canFallThrough = canFallThrough;
		this.defaultMatcher = defaultMatcher;
		this.breakMatcher = breakMatcher;
		this.returnMatcher = returnMatcher;
	}

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		List<ShallowEntity> caseEntities = extractCaseEntities(entities);

		// do not open a new scope for these nodes as all case labels share the
		// same scope
		Result caseNodes = creator.transform(caseEntities);

		String label = extractCaseLabel(entity.ownStartTokens());
		context.getCaseLabelManager().addLabeledNode(label,
				caseNodes.getEntryNode());

		if (shouldCreateSyntheticBreak(caseEntities, entities)) {
			// create a synthetic break statement for the last case rule to make
			// CFG construction in the switch rule easier
			ControlFlowNode breakNode = context.createSyntheticNode();
			for (ControlFlowNode exitNode : caseNodes.getExitNodes()) {
				ControlFlowNode.link(exitNode, breakNode);
			}
			context.getBreakNodes().add(breakNode);
		}

		List<ControlFlowNode> exitNodes = caseNodes.getExitNodes();
		if (!canFallThrough) {
			// since we cannot fall through, the exit nodes act like break
			// nodes
			context.getBreakNodes().addAll(caseNodes.getExitNodes());
			exitNodes = CollectionUtils.emptyList();
		}

		if (defaultMatcher.matches(entity)) {
			context.setDefaultCaseEntryNode(caseNodes.getEntryNode());
		} else {
			context.getCaseEntryNodes().add(caseNodes.getEntryNode());
		}

		return new Result(1 + caseEntities.size(), caseNodes.getEntryNode(),
				exitNodes);
	}

	/**
	 * Returns the label of the case statement in the given tokens.
	 */
	private String extractCaseLabel(List<IToken> tokens) {
		StringBuffer sb = new StringBuffer();
		for (IToken token : tokens) {
			sb.append(token.getText());
		}
		String label = sb.toString();
		label = StringUtils.stripPrefix(label, caseLabelPrefix);
		label = StringUtils.stripSuffix(label, caseLabelSuffix);
		return label;
	}

	/**
	 * Returns <code>true</code> if a synthetic break statement should be
	 * created and appended to the case statement.
	 */
	private boolean shouldCreateSyntheticBreak(
			List<ShallowEntity> caseEntities, List<ShallowEntity> entities) {
		if (!canFallThrough) {
			// if we cannot fall through, the last statement acts as the break
			// statement
			return false;
		}

		ShallowEntity lastEntity = CollectionUtils.getLast(caseEntities);
		if (lastEntity != null) {
			if (breakMatcher.matches(lastEntity)
					|| returnMatcher.matches(lastEntity)) {
				// is already handled by the break or return rule
				return false;
			}
		}

		boolean isLastCaseInSwitchStatement = caseEntities.size() == entities
				.size() - 1;
		return isLastCaseInSwitchStatement;
	}

	/**
	 * Returns all entities that still belong to this case statement, excluding
	 * the case entity itself but including any possible break statements at the
	 * end.
	 */
	private List<ShallowEntity> extractCaseEntities(List<ShallowEntity> entities) {
		List<ShallowEntity> caseStatements = new ArrayList<ShallowEntity>();
		for (int i = 1; i < entities.size(); i++) {
			ShallowEntity entity = entities.get(i);
			if (allCaseLabelsMatcher.matches(entity)) {
				break;
			}

			caseStatements.add(entity);
		}
		return caseStatements;
	}

}