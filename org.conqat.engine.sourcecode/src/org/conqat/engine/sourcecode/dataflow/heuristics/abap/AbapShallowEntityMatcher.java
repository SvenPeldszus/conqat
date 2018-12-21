/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapShallowEntityMatcher.java 51148 2014-11-14 13:51:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityMultiMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules.AbapRuleUtils;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Does case insensitive matching against a single subtype and/or name,
 * optionally followed by a colon (for chain statements).
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: EFFA528926441F4C208A6ABC574FA3A2
 */
public class AbapShallowEntityMatcher implements IShallowEntityMatcher {

	/** Matches the "and return" addition to "submit" statements. */
	private static final TokenPattern AND_RETURN_PATTERN = new TokenPattern()
			.sequence(ETokenType.AND, ETokenType.RETURN)
			.optional(ETokenType.DOT).endOfStream();

	/** Matches simple statements. */
	public static final IShallowEntityMatcher SIMPLE_STATEMENT_MATCHER = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			return isSimpleStatement(entity) || isVariableDeclaration(entity);
		}

		/**
		 * Returns <code>true</code> if the given entity is a variable
		 * declaration, e.g. "data" or "statics".
		 */
		private boolean isVariableDeclaration(ShallowEntity entity) {
			return AbapRuleUtils.hasSubtype(entity, SubTypeNames.DATA,
					SubTypeNames.STATICS, SubTypeNames.CONSTANTS,
					SubTypeNames.FIELD_SYMBOLS, SubTypeNames.PARAMETERS,
					SubTypeNames.TABLES, SubTypeNames.NODES);
		}

		/**
		 * Returns <code>true</code> if the given entity is a statement without
		 * children.
		 */
		private boolean isSimpleStatement(ShallowEntity entity) {
			return entity.getType() == EShallowEntityType.STATEMENT
					&& entity.getChildren().isEmpty();
		}
	};

	/** Matches the default case label "when others". */
	public static final IShallowEntityMatcher WHEN_OTHERS_MATCHER = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			if (!AbapRuleUtils.hasSubtype(entity, SubTypeNames.WHEN)) {
				return false;
			}
			return entity.ownStartTokens().get(1).getType() == ETokenType.OTHERS;
		}
	};

	/** Matches submit statements that do not return to the current report. */
	private static final IShallowEntityMatcher SUBMIT_WITHOUT_RETURN = new IShallowEntityMatcher() {

		@Override
		public boolean matches(ShallowEntity entity) {
			UnmodifiableList<IToken> tokens = entity.ownStartTokens();
			if (!AbapRuleUtils.hasSubtype(entity, SubTypeNames.SUBMIT)) {
				return false;
			}
			return !AND_RETURN_PATTERN.matches(tokens);
		}
	};

	/** Matches statements that return from the current method. */
	public static final IShallowEntityMatcher RETURN_MATCHER = new ShallowEntityMultiMatcher(
			new AbapShallowEntityMatcher(SubTypeNames.LEAVE),
			new AbapShallowEntityMatcher(SubTypeNames.RETURN),
			new AbapShallowEntityMatcher(SubTypeNames.STOP),
			new AbapShallowEntityMatcher(SubTypeNames.REJECT),
			new AbapShallowEntityMatcher(SubTypeNames.RAISE),
			SUBMIT_WITHOUT_RETURN);

	/**
	 * Matches statements that do not represent interesting control flow and are
	 * ignored.
	 */
	public static final IShallowEntityMatcher UNINTERESTING_ENTITIES = new ShallowEntityMultiMatcher(
			new AbapShallowEntityMatcher(SubTypeNames.TYPES),
			new AbapShallowEntityMatcher(SubTypeNames.NODES),
			new AbapShallowEntityMatcher(SubTypeNames.TABLES),
			new AbapShallowEntityMatcher(SubTypeNames.INCLUDE),
			new AbapShallowEntityMatcher(SubTypeNames.TYPE_POOLS),
			new AbapShallowEntityMatcher(SubTypeNames.MACRO));

	/** The subtype to match or <code>null</code>. */
	private final String subtype;

	/**
	 * Constructor.
	 * 
	 * @param subtype
	 *            the subtype to match. This parameter is case insensitive. This
	 *            matcher also accepts the given subtype followed by a colon
	 *            (i.e. chain statements).
	 */
	public AbapShallowEntityMatcher(String subtype) {
		this.subtype = subtype;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		return subtype.equalsIgnoreCase(entity.getSubtype())
				|| (subtype + ":").equalsIgnoreCase(entity.getSubtype());
	}

}
