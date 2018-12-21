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
package org.conqat.engine.sourcecode.shallowparser.languages.base;

import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for recognizing statements in a language whose statements are
 * primarily line-based. It also assumes that a semicolon may always be used to
 * terminate a statement.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: 073D1532F45028E6A6B537C526333C75
 */
public abstract class LineBasedStatementRecognizerBase<STATE extends Enum<STATE>>
		extends RecognizerBase<STATE> {

	/** Matched tokens for nesting in complex statements. */
	protected final static Map<ETokenType, ETokenType> NESTING_MATCH = new EnumMap<ETokenType, ETokenType>(
			ETokenType.class);

	static {
		NESTING_MATCH.put(LPAREN, RPAREN);
		NESTING_MATCH.put(LBRACK, RBRACK);
		NESTING_MATCH.put(LBRACE, RBRACE);
	}

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<STATE> parserState,
			List<IToken> tokens, int startOffset) {
		IToken lastToken = null;
		Stack<ETokenType> expectedClosing = new Stack<ETokenType>();

		// create a node here, so we can append function nodes
		parserState.setNode(new ShallowEntity(EShallowEntityType.STATEMENT,
				SubTypeNames.SIMPLE_STATEMENT, tokens.get(startOffset)
						.getText(), tokens, startOffset));

		while (true) {
			if (startOffset >= tokens.size()) {
				return startOffset;
			}

			IToken token = tokens.get(startOffset);
			ETokenType tokenType = token.getType();

			if (!expectedClosing.isEmpty()
					&& tokenType == expectedClosing.peek()) {
				expectedClosing.pop();
			} else if (NESTING_MATCH.containsKey(tokenType)) {
				expectedClosing.push(NESTING_MATCH.get(tokenType));
			} else if (expectedClosing.isEmpty() && tokenType == SEMICOLON) {
				return startOffset + 1;
			} else if (expectedClosing.isEmpty()
					&& startsNewStatement(token, lastToken)) {
				return startOffset;
			} else if (tokenStartsSubParse(tokenType)) {
				int next = parserState.parse(getSubParseState(), tokens,
						startOffset);
				if (next == NO_MATCH) {
					return NO_MATCH;
				}
				startOffset = next;
				lastToken = tokens.get(startOffset - 1);
				continue;
			}

			lastToken = token;
			startOffset += 1;
		}
	}

	/** Returns the state to be used for a sub parse. */
	protected abstract STATE getSubParseState();

	/**
	 * Returns true if the token signals to start a sub parse (e.g. embedded
	 * classes, functions, etc.).
	 */
	protected abstract boolean tokenStartsSubParse(ETokenType tokenType);

	/** Returns true if the given token starts a new statement. */
	protected abstract boolean startsNewStatement(IToken token, IToken lastToken);
}