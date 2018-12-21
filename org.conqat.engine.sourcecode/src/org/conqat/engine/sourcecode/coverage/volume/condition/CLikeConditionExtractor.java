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
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Extracts {@link Condition}s from {@link ShallowEntity}s of c-like languages.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51536 $
 * @ConQAT.Rating GREEN Hash: BE230B28EA4929A12C643E54AA2585E8
 */
public class CLikeConditionExtractor extends ConditionExtractorBase {

	/** Constructor. */
	public CLikeConditionExtractor(ELanguage language) {
		super(getBooleanResultOperators(language));
	}

	/**
	 * Returns the token types that correspond to operators whose result is a
	 * boolean value.
	 */
	private static Set<ETokenType> getBooleanResultOperators(ELanguage language) {
		Set<ETokenType> operators = EnumSet.of(ETokenType.ANDAND,
				ETokenType.OROR, ETokenType.EQEQ, ETokenType.NOTEQ,
				ETokenType.LT, ETokenType.GT, ETokenType.LTEQ, ETokenType.GTEQ);

		// we do not include this for C++, as there the "&" and "|" are
		// explicitly used with integers and it is discouraged to use them with
		// booleans. Hence, they are nearly never actual boolean operators.
		if (language == ELanguage.JAVA || language == ELanguage.CS) {
			operators.add(ETokenType.OR);
			operators.add(ETokenType.AND);
		}
		return operators;
	}

	/** {@inheritDoc} */
	@Override
	public Condition extractCondition(ShallowEntity entity)
			throws ConQATException {
		switch (entity.getSubtype()) {
		case SubTypeNames.IF:
		case SubTypeNames.ELSE_IF:
		case SubTypeNames.WHILE:
			return extractConditionInParenthesis(entity.ownStartTokens());
		case SubTypeNames.DO:
			return extractConditionInParenthesis(entity.ownEndTokens());
		case SubTypeNames.FOR:
			return extractConditionInSemicolons(entity.ownStartTokens());
		case SubTypeNames.FOREACH:
			return null;
		default:
			throw new ConQATException("Entity has no conditional subtype!");
		}
	}

	/**
	 * Extracts a {@link Condition} between parenthesis from the given list of
	 * tokens.
	 */
	private Condition extractConditionInParenthesis(List<IToken> tokens)
			throws ConQATException {
		int openingIndex = TokenStreamUtils.find(tokens, ETokenType.LPAREN);
		int closingIndex = TokenStreamUtils.findMatchingClosingToken(tokens,
				openingIndex + 1, ETokenType.LPAREN, ETokenType.RPAREN);

		if (openingIndex == TokenStreamUtils.NOT_FOUND
				|| closingIndex == TokenStreamUtils.NOT_FOUND) {
			throw new ConQATException("Entity has no valid embedded condition.");
		}

		return new Condition(tokens.subList(openingIndex + 1, closingIndex));
	}

	/**
	 * Extracts a {@link Condition} between two semicolons from the given list
	 * of tokens.
	 */
	private Condition extractConditionInSemicolons(List<IToken> tokens) {
		int firstSemicolon = TokenStreamUtils
				.find(tokens, ETokenType.SEMICOLON);
		int secondSemicolon = TokenStreamUtils.find(tokens,
				firstSemicolon + 1, ETokenType.SEMICOLON);

		if (firstSemicolon == TokenStreamUtils.NOT_FOUND
				|| secondSemicolon == TokenStreamUtils.NOT_FOUND) {
			// no semicolons; likely Java for-each loop
			return null;
		}

		return new Condition(
				tokens.subList(firstSemicolon + 1, secondSemicolon));
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isDoubleOperator(ETokenType type1, ETokenType type2) {
		// exclude shift operators
		return (type1 == ETokenType.LT && type2 == ETokenType.LT)
				|| (type1 == ETokenType.GT && type2 == ETokenType.GT);
	}

	/** {@inheritDoc} */
	@Override
	protected void extractGeneralConditionWithoutCall(List<IToken> tokens,
			List<Condition> conditions) {
		// in C-like languages we also have to deal with the ?/: operator, which
		// we do by splitting at both operators

		int currentStart = 0;
		for (int i = 0; i < tokens.size(); ++i) {
			ETokenType type = tokens.get(i).getType();
			if (type == ETokenType.QUESTION || type == ETokenType.COLON) {
				if (i > currentStart) {
					super.extractGeneralConditionWithoutCall(
							tokens.subList(currentStart, i), conditions);
				}
				currentStart = i + 1;
			}
		}

		if (tokens.size() > currentStart) {
			super.extractGeneralConditionWithoutCall(
					tokens.subList(currentStart, tokens.size()), conditions);
		}
	}
}
