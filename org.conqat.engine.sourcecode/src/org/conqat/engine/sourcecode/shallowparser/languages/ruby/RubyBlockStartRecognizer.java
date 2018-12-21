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
package org.conqat.engine.sourcecode.shallowparser.languages.ruby;

import static org.conqat.engine.sourcecode.shallowparser.languages.ruby.RubyShallowParser.ERubyParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.ruby.RubyShallowParser.ERubyParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for simple statements in Ruby.
 * http://jigargosar.blogspot.de/2006/01/ruby-can-ruby-statement-break-into.html
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating RED Hash: C8C4F995833C521D077372D341D4E7ED
 */
/* package */class RubyBlockStartRecognizer extends
		RecognizerBase<ERubyParserStates> {

	/** Matched tokens for nesting in complex statements. */
	private final static Map<ETokenType, ETokenType> NESTING_MATCH = new EnumMap<ETokenType, ETokenType>(
			ETokenType.class);

	static {
		NESTING_MATCH.put(LPAREN, RPAREN);
		NESTING_MATCH.put(LBRACK, RBRACK);
		NESTING_MATCH.put(LBRACE, RBRACE);
	}

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<ERubyParserStates> parserState,
			List<IToken> tokens, int startOffset) {
		IToken lastToken = null;
		Stack<ETokenType> expectedClosing = new Stack<ETokenType>();

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
			} else if (expectedClosing.isEmpty()
					&& startsNewStatement(token, lastToken)) {
				return startOffset;
			}

			if (tokenType == ETokenType.STRING_LITERAL) {
				int next = parserState.parse(ANY, tokens, startOffset);
				if (next == NO_MATCH) {
					return NO_MATCH;
				}
				startOffset = next;
				lastToken = null;
				continue;
			}

			lastToken = token;
			startOffset += 1;
		}
	}

	/** Returns true if the given token starts a new statement. */
	private boolean startsNewStatement(IToken token, IToken lastToken) {
		if (lastToken == null) {
			return false;
		}

		ETokenType lastTokenType = lastToken.getType();
		if (lastTokenType == ETokenType.SEMICOLON
				|| lastTokenType == ETokenType.THEN
				|| lastTokenType == ETokenType.DO) {
			return true;
		}

		// same line => no new statement
		if (lastToken.getLineNumber() == token.getLineNumber()) {
			return false;
		}

		ETokenClass lastTokenClass = lastTokenType.getTokenClass();
		if (lastTokenClass == ETokenClass.OPERATOR
				|| lastTokenType == ETokenType.BACKSLASH) {
			return false;
		}

		return true;
	}
}