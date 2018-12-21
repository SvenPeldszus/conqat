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
package org.conqat.engine.sourcecode.shallowparser.languages.cpp;

import org.conqat.engine.sourcecode.shallowparser.framework.SequenceRecognizer.ITokenMatcher;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ETokenType.ETokenClass;

/**
 * Matches the string "asm", "_asm", or "__asm".
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: EDBAB526024C49E2BAA65802B06667B0
 */
/* package */class ASMIdentifierMatcher implements ITokenMatcher {

	/** {@inheritDoc} */
	@Override
	public boolean matches(IToken token) {
		return token.getType().getTokenClass() == ETokenClass.IDENTIFIER
				&& ("asm".equals(token.getText())
						|| "_asm".equals(token.getText()) || "__asm"
							.equals(token.getText()));
	}
}