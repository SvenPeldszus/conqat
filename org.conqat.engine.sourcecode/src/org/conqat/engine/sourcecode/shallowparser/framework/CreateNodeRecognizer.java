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

import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.IToken;

/**
 * The recognizer used to implement creation of nodes in the parse tree, i.e.
 * ShallowEntities.
 * 
 * @param <STATE>
 *            the enum used for describing parse states.
 * 
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49839 $
 * @ConQAT.Rating GREEN Hash: 94E4C990B3C8E129AF600F1588E5B0C2
 */
/* package */class CreateNodeRecognizer<STATE extends Enum<STATE>> extends
		RecognizerBase<STATE> {

	/** The type. */
	private final EShallowEntityType type;

	/**
	 * The subtype, which should be either a string or an integer or an array of
	 * these.
	 */
	private final Object subtype;

	/**
	 * The name, which should be either a string or an integer or an array of
	 * these.
	 */
	private final Object name;

	/** Offset to apply to the node start position. */
	private final int offset;

	/**
	 * Constructor. The subtype and name can either be null, a constant name
	 * (String), an index into the token stream (int), a {@link Region} of the
	 * token stream, or an array (indicating multiple of those mentioned before.
	 * 
	 * @param offset
	 *            An offset (in terms of number of tokens) by which to move the
	 *            start of the node to the right.
	 */
	public CreateNodeRecognizer(EShallowEntityType type, Object subtype,
			Object name, int offset) {
		checkNameParameter(subtype);
		checkNameParameter(name);

		this.type = type;
		this.subtype = subtype;
		this.name = name;
		this.offset = offset;
	}

	/**
	 * Checks the type of a naming parameter. This can either be null, a
	 * constant name (String), an index into the token stream (int), a
	 * {@link Region} of the token stream, or an array (indicating multiple of
	 * those mentioned before.
	 */
	private void checkNameParameter(Object subtype) {
		CCSMPre.isTrue(subtype == null || subtype instanceof String
				|| subtype instanceof Integer || subtype instanceof Region
				|| subtype.getClass().isArray(),
				"Parameter must be null, String, int, Region or array!");
	}

	/** {@inheritDoc} */
	@Override
	public int matchesLocally(ParserState<STATE> parserState,
			List<IToken> tokens, int startOffset) {
		String resolvedSubtype = RecognizerUtils.resolveName(tokens,
				parserState, startOffset, subtype);
		String resolvedName = RecognizerUtils.resolveName(tokens, parserState,
				startOffset, name);
		parserState.setNode(new ShallowEntity(type, resolvedSubtype,
				resolvedName, tokens, parserState.getCurrentMatchStart()
						+ offset));
		return startOffset;
	}

	/** {@inheritDoc} */
	@Override
	public int matches(ParserState<STATE> parserState, List<IToken> tokens,
			int startOffset) {
		// Make sure that we always return a match after creating a node.
		// The "max" is used to handle the case of super.matches() returning -1
		return Math.max(startOffset,
				super.matches(parserState, tokens, startOffset));
	}
}