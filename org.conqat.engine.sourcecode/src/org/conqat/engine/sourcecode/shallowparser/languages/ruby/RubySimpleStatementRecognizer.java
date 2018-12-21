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
import static org.conqat.lib.scanner.ETokenType.DO;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ELSIF;
import static org.conqat.lib.scanner.ETokenType.END;
import static org.conqat.lib.scanner.ETokenType.ENSURE;
import static org.conqat.lib.scanner.ETokenType.RBRACE;

import java.util.EnumSet;
import java.util.List;

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
 * @ConQAT.Rating RED Hash: B8CCB0EA31D73B2BA3BB03DBC5CA91BF
 */
/* package */class RubySimpleStatementRecognizer extends
		RecognizerBase<ERubyParserStates> {

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<ERubyParserStates> parserState,
			List<IToken> tokens, int startOffset) {
		IToken lastToken = null;

		while (true) {
			if (startOffset >= tokens.size()) {
				return startOffset;
			}

			IToken token = tokens.get(startOffset);
			ETokenType tokenType = token.getType();

			if (startsNewStatement(token, lastToken)) {
				return startOffset;
			}

			if (tokenType == DO || tokenType == ETokenType.LBRACE
					|| tokenType == ETokenType.STRING_LITERAL) {
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
		ETokenType tokenType = token.getType();
		// don't consume
		if (EnumSet.of(END, ELSE, ELSIF, ENSURE, RBRACE).contains(tokenType)) {
			return true;
		}

		// consume
		if (lastTokenType == ETokenType.SEMICOLON) {
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