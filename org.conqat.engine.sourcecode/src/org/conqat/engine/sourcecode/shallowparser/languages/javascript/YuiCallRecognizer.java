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
package org.conqat.engine.sourcecode.shallowparser.languages.javascript;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.javascript.JavaScriptShallowParser.EJavaScriptParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for calls of the global YUI object from the YUI framework.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51392 $
 * @ConQAT.Rating GREEN Hash: E579C971A5A38A4D381DF15D62D023EF
 */
public class YuiCallRecognizer extends RecognizerBase<EJavaScriptParserStates> {

	/** Name of the global YUI object. */
	private static final String YUI = "YUI";

	/** The method name. */
	private final String methodName;

	/** Constructor. */
	public YuiCallRecognizer(String methodName) {
		this.methodName = methodName;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This matches either "YUI.[methodName]" or "YUI().[methodName]".
	 */
	@Override
	protected int matchesLocally(
			ParserState<EJavaScriptParserStates> parserState,
			List<IToken> tokens, int startOffset) {

		if (tokens.size() - startOffset < 3) {
			return NO_MATCH;
		}

		if (!isIdentifier(tokens.get(startOffset), YUI)) {
			return NO_MATCH;
		}

		// optionally skip "()" after "YUI"
		if (tokens.get(startOffset + 1).getType() == ETokenType.LPAREN) {
			if (tokens.get(startOffset + 2).getType() != ETokenType.RPAREN
					|| tokens.size() - startOffset < 5) {
				return NO_MATCH;
			}
			startOffset += 2;
		}

		if (tokens.get(startOffset + 1).getType() != ETokenType.DOT) {
			return NO_MATCH;
		}

		if (!isIdentifier(tokens.get(startOffset + 2), methodName)) {
			return NO_MATCH;
		}

		return startOffset + 3;
	}

	/** Returns whether the given token is an identifier of given name. */
	private boolean isIdentifier(IToken token, String name) {
		return token.getType() == ETokenType.IDENTIFIER
				&& token.getText().equals(name);
	}
}
