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
package org.conqat.engine.sourcecode.shallowparser.languages.java;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer that finds anonymous classes and performs parsing within this
 * class.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: 9412D540139B53C5DCBB8BABBE48A2E5
 */
/* package */class JavaAnonymousClassRecognizer extends
		RecognizerBase<EGenericParserStates> {

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<EGenericParserStates> parserState,
			List<IToken> tokens, int startOffset) {

		int currentOffset = startOffset;
		if (!TokenStreamUtils.tokenTypesAt(tokens, currentOffset,
				ETokenType.NEW, ETokenType.IDENTIFIER)) {
			return NO_MATCH;
		}
		currentOffset += 2;

		// skip fully qualified names
		while (TokenStreamUtils.tokenTypesAt(tokens, currentOffset,
				ETokenType.DOT, ETokenType.IDENTIFIER)) {
			currentOffset += 2;
		}

		// skip generics specification
		if (TokenStreamUtils.tokenTypesAt(tokens, currentOffset, ETokenType.LT)) {
			currentOffset = TokenStreamUtils.findMatchingClosingToken(tokens,
					currentOffset + 1, ETokenType.LT, ETokenType.GT);
			if (currentOffset == TokenStreamUtils.NOT_FOUND) {
				return NO_MATCH;
			}
			currentOffset += 1;
		}

		// expect and skip parentheses
		if (!TokenStreamUtils.tokenTypesAt(tokens, currentOffset,
				ETokenType.LPAREN)) {
			return NO_MATCH;
		}
		currentOffset = TokenStreamUtils.findMatchingClosingToken(tokens,
				currentOffset + 1, ETokenType.LPAREN, ETokenType.RPAREN);
		if (currentOffset == TokenStreamUtils.NOT_FOUND) {
			return NO_MATCH;
		}
		currentOffset += 1;

		if (TokenStreamUtils.tokenTypesAt(tokens, currentOffset,
				ETokenType.LBRACE)) {
			return parserState.parse(EGenericParserStates.IN_EXPRESSION,
					tokens, startOffset);
		}

		return NO_MATCH;
	}

}
