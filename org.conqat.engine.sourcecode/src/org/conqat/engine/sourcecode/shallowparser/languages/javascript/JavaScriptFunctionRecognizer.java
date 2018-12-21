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

import static org.conqat.lib.scanner.ETokenType.FUNCTION;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.javascript.JavaScriptShallowParser.EJavaScriptParserStates;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer that finds local functions and performs parsing starting from the
 * function.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: F5D87A8FA32EA0BB9BF0CE9A8C157C06
 */
/* package */class JavaScriptFunctionRecognizer extends
		RecognizerBase<EJavaScriptParserStates> {

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(
			ParserState<EJavaScriptParserStates> parserState,
			List<IToken> tokens, int startOffset) {
		if (tokens.get(startOffset).getType() == FUNCTION) {
			return parserState.parse(EJavaScriptParserStates.ANY, tokens,
					startOffset);
		}
		return NO_MATCH;
	}
}