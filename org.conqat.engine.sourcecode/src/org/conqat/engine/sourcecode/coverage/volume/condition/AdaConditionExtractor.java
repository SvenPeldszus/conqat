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

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Extracts {@link Condition}s from {@link ShallowEntity}s of c-like languages.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51097 $
 * @ConQAT.Rating GREEN Hash: 66014154E853DF75C42421E4C1A089E5
 */
public class AdaConditionExtractor extends ConditionExtractorBase {

	/** Constructor. */
	public AdaConditionExtractor() {
		super(EnumSet.of(ETokenType.AND, ETokenType.OR, ETokenType.XOR,
				ETokenType.EQ, ETokenType.NOTEQ, ETokenType.LT, ETokenType.GT,
				ETokenType.LTEQ, ETokenType.GTEQ));
	}

	/** {@inheritDoc} */
	@Override
	public Condition extractCondition(ShallowEntity entity)
			throws ConQATException {
		switch (entity.getSubtype()) {
		case SubTypeNames.IF:
		case SubTypeNames.ELSIF:
		case SubTypeNames.WHILE:
			return stripFirstAndLast(entity.ownStartTokens(), 1);
		case SubTypeNames.FOR:
			// for has no condition, as it just counts
			return null;
		case SubTypeNames.LOOP:
			// loop itself has no condition; they are in the exit when clauses
			return null;
		case SubTypeNames.EXIT:
			return stripFirstAndLast(entity.includedTokens(), 2);
		default:
			throw new ConQATException("Entity has no conditional subtype: "
					+ entity.getSubtype());
		}
	}

	/**
	 * Returns all but the first and last tokens. Returns <code>null</code> if
	 * there would be no tokens remaining after stripping.
	 *
	 * @param numFirst
	 *            the number of tokens to strip from the beginning.
	 */
	private Condition stripFirstAndLast(List<IToken> tokens, int numFirst) {
		if (tokens.size() < numFirst + 2) {
			return null;
		}
		return new Condition(tokens.subList(numFirst, tokens.size() - 1));
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isDoubleOperator(ETokenType type1, ETokenType type2) {
		// exclude arrow operator
		return type1 == ETokenType.EQ && type2 == ETokenType.GT;
	}

}
