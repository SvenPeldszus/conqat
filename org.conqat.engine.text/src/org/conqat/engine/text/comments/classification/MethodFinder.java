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
package org.conqat.engine.text.comments.classification;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.text.comments.utils.MemberExtractor;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Helper class to find next method/field definition
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48833 $
 * @ConQAT.Rating GREEN Hash: 0023D7FA3BABACC3AFFEB85EC2498C3F
 */
public class MethodFinder {

	/** Complete token list. */
	private final List<IToken> tokenList;

	/** Extracts methods and fields from a class. */
	private final MemberExtractor memberExtractor;

	/** Constructor */
	public MethodFinder(ITokenElement element, List<IToken> tokenList)
			throws ConQATException {
		this.tokenList = tokenList;
		this.memberExtractor = new MemberExtractor(element, tokenList);
	}

	/**
	 * Returns the name of the next member and the distance (measured in token
	 * numbers) from the given index of a token list to the next member
	 * definition (returns "" and -1 if an other comment preceeds the next
	 * member or no member is found)
	 */
	private Pair<String, Integer> getNameAndDistanceToNextMember(
			int tokenListIndex, int lineNumber, boolean stopAtComments) {
		// search in same line
		for (int i = tokenListIndex - 1; i >= 0; i--) {
			IToken token = tokenList.get(i);
			if (token.getLineNumber() == lineNumber
					&& memberExtractor.containsMemberIdentifier(
							token.getText(), tokenList.get(i).getOffset())) {
				return new Pair<String, Integer>(token.getText(), 0);
			} else if (token.getLineNumber() < lineNumber) {
				break;
			}
		}

		// search from the current position forward
		for (int i = tokenListIndex + 1; i < tokenList.size(); i++) {
			if (stopAtComments
					&& tokenList.get(i).getType().getTokenClass()
							.equals(ETokenClass.COMMENT)) {
				return new Pair<String, Integer>(StringUtils.EMPTY_STRING, -1);
			}

			if (memberExtractor.containsMemberIdentifier(tokenList.get(i)
					.getText(), tokenList.get(i).getOffset())) {
				return new Pair<String, Integer>(tokenList.get(i).getText(), i
						- tokenListIndex);
			}

		}
		return new Pair<String, Integer>(StringUtils.EMPTY_STRING, -1);
	}

	/**
	 * Returns the name of the next method or field definition or an empty
	 * string if none is found.
	 * 
	 * Will stop stop searching if another comment is found first.
	 */
	public String getNextDefinition(int tokenListIndex) {
		return getNextDefinition(tokenListIndex, true);
	}

	/**
	 * Returns the name of the next method or field definition or an empty
	 * string if none is found.
	 * 
	 * @param stopAtComments
	 *            Whether to stop searching if another comment is found
	 */
	public String getNextDefinition(int tokenListIndex, boolean stopAtComments) {
		return getNameAndDistanceToNextMember(tokenListIndex,
				tokenList.get(tokenListIndex).getLineNumber(), stopAtComments)
				.getFirst();
	}

	/**
	 * Returns the distance (measured in tokens) to the next method or field
	 * definition or -1 if none is found, or is preceeded by another comment.
	 */
	public int getDistanceToNextDefinition(int tokenListIndex) {
		return getDistanceToNextDefinition(tokenListIndex, true);
	}

	/**
	 * Returns the distance (measured in tokens) to the next method or field
	 * definition or -1 if none is found.
	 * 
	 * @param stopAtComments
	 *            Whether to stop searching if another comment is found.
	 */
	public int getDistanceToNextDefinition(int tokenListIndex,
			boolean stopAtComments) {
		return getNameAndDistanceToNextMember(tokenListIndex,
				tokenList.get(tokenListIndex).getLineNumber(), stopAtComments)
				.getSecond();
	}

	/**
	 * Returns the member extractor.
	 */
	public MemberExtractor getMemberExtractor() {
		return memberExtractor;
	}
}
