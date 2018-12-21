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
package org.conqat.engine.sourcecode.shallowparser.framework;

import java.util.List;

import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Recognizer for skipping optional nested structures.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: EEDE1CAA93F1AD00B709948802D00B43
 */
/* package */class OptionalNestedRecognizer<STATE extends Enum<STATE>> extends
		RecognizerBase<STATE> {

	/** Type that opens a nesting level. */
	private final ETokenType open;

	/** Type that closes a nesting level. */
	private final ETokenType close;

	/** The sub recognizer applied to the skipped region (may be null). */
	private RecognizerBase<STATE> subRecognizer;

	/** Constructor. */
	public OptionalNestedRecognizer(ETokenType open, ETokenType close,
			RecognizerBase<STATE> subRecognizer) {
		this.open = open;
		this.close = close;
		this.subRecognizer = subRecognizer;
	}

	/** {@inheritDoc} */
	@Override
	protected int matchesLocally(ParserState<STATE> parserState,
			List<IToken> tokens, int startOffset) {

		// nothing to skip
		if (startOffset >= tokens.size()
				|| tokens.get(startOffset).getType() != open) {
			return startOffset;
		}

		int depth = 1;
		startOffset += 1;

		while (startOffset < tokens.size() && depth > 0) {

			// this has to run first, so we do not change nesting count if the
			// sub recognizer swallows open/close tokens.
			if (subRecognizer != null) {
				int next = subRecognizer.matches(parserState, tokens,
						startOffset);
				if (next != NO_MATCH) {
					startOffset = next;
					continue;
				}
			}

			if (tokens.get(startOffset).getType() == open) {
				depth += 1;
			} else if (tokens.get(startOffset).getType() == close) {
				depth -= 1;
			}

			startOffset += 1;
		}

		if (depth > 0) {
			return NO_MATCH;
		}
		return startOffset;
	}
}
