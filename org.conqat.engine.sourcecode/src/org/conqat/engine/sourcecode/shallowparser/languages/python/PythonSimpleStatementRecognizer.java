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
package org.conqat.engine.sourcecode.shallowparser.languages.python;

import static org.conqat.engine.sourcecode.shallowparser.languages.python.PythonShallowParser.EPythonParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.DEDENT;
import static org.conqat.lib.scanner.ETokenType.DEF;

import org.conqat.engine.sourcecode.shallowparser.languages.base.LineBasedStatementRecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.python.PythonShallowParser.EPythonParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for simple statements in Python.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49567 $
 * @ConQAT.Rating GREEN Hash: 6370DB8346402B87AC3E44239138055A
 */
public class PythonSimpleStatementRecognizer extends
		LineBasedStatementRecognizerBase<EPythonParserStates> {

	/** {@inheritDoc} */
	@Override
	protected EPythonParserStates getSubParseState() {
		return ANY;
	}

	/** {@inheritDoc} */
	@Override
	protected boolean tokenStartsSubParse(ETokenType tokenType) {
		return (tokenType == DEF || tokenType == CLASS);
	}

	/** {@inheritDoc} */
	@Override
	protected boolean startsNewStatement(IToken token, IToken lastToken) {
		ETokenType tokenType = token.getType();
		return tokenType == ETokenType.EOL || tokenType == DEDENT;
	}
}
