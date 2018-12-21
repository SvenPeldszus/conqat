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

import java.lang.reflect.Array;
import java.util.List;

import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.IToken;

/**
 * Utility methods for writing recognizers.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50138 $
 * @ConQAT.Rating GREEN Hash: 77F25420A5EBE168620D8F3305ADABBE
 */
public class RecognizerUtils {

	/**
	 * Resolves a name based on the token stream and the parser state. The
	 * "name" can either be null, a constant name (String), an index into the
	 * token stream (int), a {@link Region} of the token stream, or an array
	 * (indicating multiple of those mentioned before.
	 */
	public static <STATE extends Enum<STATE>> String resolveName(
			List<IToken> tokens, ParserState<STATE> parserState,
			int startOffset, Object name) {
		if (name == null) {
			return null;
		}

		if (name instanceof String) {
			return (String) name;
		}

		if (name instanceof Number) {
			int index = ((Number) name).intValue();
			return normalizeText(tokens.get(resolveIndex(parserState,
					startOffset, index)));
		}

		if (name instanceof Region) {
			Region region = (Region) name;
			int start = resolveIndex(parserState, startOffset,
					region.getStart());
			int end = resolveIndex(parserState, startOffset, region.getEnd());
			StringBuilder sb = new StringBuilder();
			for (int i = start; i <= end; ++i) {
				if (i > start
						&& !region.getOrigin().equals(Region.UNKNOWN_ORIGIN)) {
					sb.append(region.getOrigin());
				}
				sb.append(normalizeText(tokens.get(i)));
			}
			return sb.toString();
		}

		if (name.getClass().isArray()) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < Array.getLength(name); ++i) {
				if (i > 0) {
					sb.append(" ");
				}
				sb.append(resolveName(tokens, parserState, startOffset,
						Array.get(name, i)));
			}
			return sb.toString();
		}

		throw new AssertionError("Unexpected type in resolving of name: "
				+ name.getClass());
	}

	/** Resolves the index relative to the parser state. */
	private static <STATE extends Enum<STATE>> int resolveIndex(
			ParserState<STATE> parserState, int startOffset, int index) {
		if (index >= 0) {
			index = parserState.getCurrentReferencePosition() + index;
		} else {
			// we need addition here, as index is negative
			index = startOffset + index;
		}
		return index;
	}

	/**
	 * Returns the token text. For case-insensitive languages, it is converted
	 * to lower case.
	 */
	private static String normalizeText(IToken token) {
		if (token.getLanguage().isCaseSensitive()) {
			return token.getText();
		}
		return token.getText().toLowerCase();
	}
}
