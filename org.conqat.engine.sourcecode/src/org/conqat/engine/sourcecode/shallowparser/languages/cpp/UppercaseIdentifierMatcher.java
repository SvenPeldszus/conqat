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

import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;

import java.util.regex.Pattern;

import org.conqat.engine.sourcecode.shallowparser.framework.SequenceRecognizer.ITokenMatcher;
import org.conqat.lib.scanner.IToken;

/**
 * Matches all uppercase identifiers with at least 4 characters. This is part of
 * the heuristic to recognize macros, so the limit is somewhat arbitrary.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: 0AC0070322667D38856F27E55B890C1B
 */
/* package */class UppercaseIdentifierMatcher implements ITokenMatcher {

	/**
	 * Pattern containing all valid characters for C++ uppercase identifiers.
	 */
	private static final Pattern UPPERCASE_PATTERN = Pattern
			.compile("[_A-Z0-9]+");

	/** {@inheritDoc} */
	@Override
	public boolean matches(IToken token) {
		return token.getType() == IDENTIFIER && token.getText().length() > 3
				&& UPPERCASE_PATTERN.matcher(token.getText()).matches();
	}
}