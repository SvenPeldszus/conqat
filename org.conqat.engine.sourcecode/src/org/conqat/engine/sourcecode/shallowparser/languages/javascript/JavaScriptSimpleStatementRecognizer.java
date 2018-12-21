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

import static org.conqat.engine.sourcecode.shallowparser.languages.javascript.JavaScriptShallowParser.EJavaScriptParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.BREAK;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.CONTINUE;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.FUNCTION;
import static org.conqat.lib.scanner.ETokenType.MINUSMINUS;
import static org.conqat.lib.scanner.ETokenType.PLUSPLUS;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RETURN;
import static org.conqat.lib.scanner.ETokenType.THROW;

import org.conqat.engine.sourcecode.shallowparser.languages.base.LineBasedStatementRecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.javascript.JavaScriptShallowParser.EJavaScriptParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for simple statements in JavaScript. We need a separate recognizer
 * as the rules for statement continuation are non-trivial due to the optional
 * semicolon. A good introduction to the topic can be found <a href=
 * "http://blog.izs.me/post/2353458699/an-open-letter-to-javascript-leaders-regarding"
 * >here</a>.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: 957D1839CD07FB8C3AFBFC789E27746A
 */
/* package */class JavaScriptSimpleStatementRecognizer extends
		LineBasedStatementRecognizerBase<EJavaScriptParserStates> {

	/** {@inheritDoc} */
	@Override
	protected EJavaScriptParserStates getSubParseState() {
		return ANY;
	}

	/** {@inheritDoc} */
	@Override
	protected boolean tokenStartsSubParse(ETokenType tokenType) {
		return tokenType == FUNCTION;
	}

	/** {@inheritDoc} */
	@Override
	protected boolean startsNewStatement(IToken token, IToken lastToken) {
		ETokenType tokenType = token.getType();
		if (tokenType == RBRACE) {
			return true;
		}

		if (lastToken == null) {
			return false;
		}

		// same line => no new statement
		if (lastToken.getLineNumber() == token.getLineNumber()) {
			return false;
		}

		ETokenType lastTokenType = lastToken.getType();

		// jump statements always end at a new line
		if (lastTokenType == RETURN || lastTokenType == BREAK
				|| lastTokenType == CONTINUE || lastTokenType == THROW) {
			return true;
		}

		// ++ and -- bind to next line
		if (tokenType == PLUSPLUS || tokenType == MINUSMINUS) {
			return true;
		}

		// continue statement is line ends with '.' or ','
		if (lastTokenType == DOT || lastTokenType == COMMA) {
			return false;
		}

		// continue statement if line ends in operator or next line starts
		// with operator or delimiter
		if (lastTokenType.getTokenClass() == ETokenClass.OPERATOR
				|| tokenType.getTokenClass() == ETokenClass.OPERATOR
				|| tokenType.getTokenClass() == ETokenClass.DELIMITER) {
			return false;
		}

		return true;
	}
}
