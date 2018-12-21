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
package org.conqat.engine.sourcecode.analysis.cpp;

import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 51536 $
 * @ConQAT.Rating GREEN Hash: 9CFB328A37E0630289452882FFA6626C
 */
@AConQATProcessor(description = "Checks that exceptions are always caught by reference.")
public class CppCatchExceptionByReferenceAnalyzer extends
		CppFindingAnalyzerBase {

	/** The name of the group to put findings into. */
	public static final String FINDINGS_GROUP_NAME = "Catch exceptions by reference.";

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {
		for (ShallowEntity statement : ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.STATEMENT)) {

			UnmodifiableList<IToken> tokens = statement.includedTokens();
			if (!tokens.isEmpty()
					&& tokens.get(0).getType() == ETokenType.CATCH) {
				checkCatchClause(element, statement, tokens);
			}
		}
	}

	/** Checks a given catch clause whether it does catch-by-reference. */
	private void checkCatchClause(ITokenElement element,
			ShallowEntity catchClause, List<IToken> tokens)
			throws ConQATException {

		int parenOpenIndex = TokenStreamUtils
				.find(tokens, 1, ETokenType.LPAREN);
		int parenCloseIndex = TokenStreamUtils.find(tokens, parenOpenIndex,
				ETokenType.RPAREN);

		if (parenOpenIndex < 0 || parenCloseIndex < 0
				|| parenCloseIndex < parenOpenIndex) {
			// ignore malformed catch clause
			return;
		}

		if (TokenStreamUtils.tokenStreamContains(tokens, parenOpenIndex,
				parenCloseIndex, ETokenType.ELLIPSIS)) {
			// ellipsis is valid catch clause
			return;
		}

		if (!TokenStreamUtils.tokenStreamContains(tokens, parenOpenIndex,
				parenCloseIndex, ETokenType.AND)) {
			createFindingForEntityStart(
					"Exception must be caught by reference!", element,
					catchClause);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}
}
