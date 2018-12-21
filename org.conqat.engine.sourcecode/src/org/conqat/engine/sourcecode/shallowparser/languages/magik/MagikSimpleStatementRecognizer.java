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
package org.conqat.engine.sourcecode.shallowparser.languages.magik;

import static org.conqat.engine.sourcecode.shallowparser.languages.magik.MagikShallowParser.EMagikParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.BLOCK;
import static org.conqat.lib.scanner.ETokenType.CATCH;
import static org.conqat.lib.scanner.ETokenType.CLONE;
import static org.conqat.lib.scanner.ETokenType.CONTINUE;
import static org.conqat.lib.scanner.ETokenType.ELIF;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ENDCATCH;
import static org.conqat.lib.scanner.ETokenType.ENDLOOP;
import static org.conqat.lib.scanner.ETokenType.ENDMETHOD;
import static org.conqat.lib.scanner.ETokenType.ENDTRY;
import static org.conqat.lib.scanner.ETokenType.END_BLOCK;
import static org.conqat.lib.scanner.ETokenType.END_IF;
import static org.conqat.lib.scanner.ETokenType.END_LOCK;
import static org.conqat.lib.scanner.ETokenType.END_PROC;
import static org.conqat.lib.scanner.ETokenType.END_PROTECT;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.LEAVE;
import static org.conqat.lib.scanner.ETokenType.LOCK;
import static org.conqat.lib.scanner.ETokenType.LOOP;
import static org.conqat.lib.scanner.ETokenType.OVER;
import static org.conqat.lib.scanner.ETokenType.PROC;
import static org.conqat.lib.scanner.ETokenType.PROTECT;
import static org.conqat.lib.scanner.ETokenType.RETURN;
import static org.conqat.lib.scanner.ETokenType.SELF;
import static org.conqat.lib.scanner.ETokenType.THROW;
import static org.conqat.lib.scanner.ETokenType.TRY;
import static org.conqat.lib.scanner.ETokenType.UNSET;

import java.util.EnumSet;
import java.util.Set;

import org.conqat.engine.sourcecode.shallowparser.languages.base.LineBasedStatementRecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.magik.MagikShallowParser.EMagikParserStates;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for simple statements in Magik. This handles the statements ending
 * at the end of line and allows parsing into sub expressions, such as local
 * procedures within statements.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49551 $
 * @ConQAT.Rating GREEN Hash: 896CADA6804554141ADA73E3002FC594
 */
public class MagikSimpleStatementRecognizer extends
		LineBasedStatementRecognizerBase<EMagikParserStates> {

	/**
	 * Token types that end a statement when at the end of line even though they
	 * are keywords.
	 */
	private static final Set<ETokenType> VALID_STATEMENT_END_TOKEN_TYPES = EnumSet
			.of(LEAVE, RETURN, THROW, CONTINUE, SELF, CLONE, UNSET, END_BLOCK,
					ENDCATCH, END_IF, END_LOCK, ENDLOOP, ENDMETHOD, END_PROC,
					END_PROTECT, ENDTRY);

	/** Token types that end a statement even in the same line. */
	private static final Set<ETokenType> SAME_LINE_STATEMENT_DELIMITER_TOKEN_TYPES = EnumSet
			.of(ELIF, ELSE, END_BLOCK, ENDCATCH, END_IF, END_LOCK, ENDLOOP,
					ENDMETHOD, END_PROC, END_PROTECT, ENDTRY);

	/** Token types that start a sub parse. */
	private static final Set<ETokenType> SUB_PARSE_TOKEN_TYPES = EnumSet.of(IF,
			FOR, OVER, LOOP, BLOCK, PROC, TRY, CATCH, PROTECT, LOCK);

	/** {@inheritDoc} */
	@Override
	protected EMagikParserStates getSubParseState() {
		return ANY;
	}

	/** {@inheritDoc} */
	@Override
	protected boolean tokenStartsSubParse(ETokenType tokenType) {
		return SUB_PARSE_TOKEN_TYPES.contains(tokenType);
	}

	/** {@inheritDoc} */
	@Override
	protected boolean startsNewStatement(IToken token, IToken lastToken) {
		if (lastToken == null) {
			return false;
		}

		// same line => usually no new statement
		if (lastToken.getLineNumber() == token.getLineNumber()) {
			return SAME_LINE_STATEMENT_DELIMITER_TOKEN_TYPES.contains(token
					.getType());
		}

		// if the previous line ended in an operator or a keyword, we can not
		// end here
		ETokenType lastTokenType = lastToken.getType();
		ETokenClass lastTokenClass = lastTokenType.getTokenClass();
		if (lastTokenClass == ETokenClass.OPERATOR
				|| lastTokenClass == ETokenClass.KEYWORD
				|| lastTokenType == ETokenType.DOT) {
			return VALID_STATEMENT_END_TOKEN_TYPES.contains(lastTokenType);
		}

		return true;
	}
}
