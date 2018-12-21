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
package org.conqat.engine.sourcecode.shallowparser.languages.cs;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer that finds delegate methods and lambdas and performs parsing
 * within them.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: 124658729A87EB35672CEEA41B9928A6
 */
/* package */class CsDelegateAndLambdaRecognizer extends
		RecognizerBase<EGenericParserStates> {

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<EGenericParserStates> parserState,
			List<IToken> tokens, int startOffset) {

		boolean delegateStarts = TokenStreamUtils.tokenTypesAt(tokens,
				startOffset, ETokenType.DELEGATE, ETokenType.LPAREN);
		boolean simpleLambdaStarts = TokenStreamUtils.tokenTypesAt(tokens,
				startOffset, ETokenType.IDENTIFIER, ETokenType.DOUBLE_ARROW);

		if (delegateStarts || simpleLambdaStarts) {
			return parserState.parse(EGenericParserStates.IN_EXPRESSION,
					tokens, startOffset);
		}

		if (TokenStreamUtils.tokenTypesAt(tokens, startOffset,
				ETokenType.LPAREN)) {
			int closingPosition = TokenStreamUtils.findMatchingClosingToken(
					tokens, startOffset + 1, ETokenType.LPAREN,
					ETokenType.RPAREN);
			if (closingPosition == TokenStreamUtils.NOT_FOUND) {
				return NO_MATCH;
			}

			if (TokenStreamUtils.tokenTypesAt(tokens, closingPosition + 1,
					ETokenType.DOUBLE_ARROW)) {
				return parserState.parse(EGenericParserStates.IN_EXPRESSION,
						tokens, startOffset);
			}
		}

		return NO_MATCH;
	}

}
