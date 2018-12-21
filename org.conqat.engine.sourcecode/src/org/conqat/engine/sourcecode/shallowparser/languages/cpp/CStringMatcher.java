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

import static org.conqat.lib.scanner.ETokenType.STRING_LITERAL;

import org.conqat.engine.sourcecode.shallowparser.framework.SequenceRecognizer.ITokenMatcher;
import org.conqat.lib.scanner.IToken;

/**
 * Matches the string "C" or "C++".
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: 52C99EA13057932F13C6BDA9EC8421CD
 */
/* package */class CStringMatcher implements ITokenMatcher {

	/** {@inheritDoc} */
	@Override
	public boolean matches(IToken token) {
		return token.getType() == STRING_LITERAL
				&& ("\"C\"".equals(token.getText()) || "\"C++\"".equals(token
						.getText()));
	}
}