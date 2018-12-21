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
package org.conqat.engine.sourcecode.analysis.clike;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.commons.findings.FindingCategoryNames;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.analysis.TokenElementFindingAnalyzerBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 *
 * @author $Author: hummelb $
 * @version $Rev: 50600 $
 * @ConQAT.Rating GREEN Hash: E4356B6AE9704B9901628B7AE2EC59DF
 */
@AConQATProcessor(description = "This processor reports empty blocks in "
		+ "source files as findings. Blocks containing only comments are "
		+ "not reported.")
public class EmptyBlocksAnalyzer extends TokenElementFindingAnalyzerBase {

	/** The name to use for the findings group. */
	public static final String FINDINGS_GROUP_NAME = "Empty blocks";

	/**
	 * Set of tokens after which an opening brace is ignored.
	 */
	private static final Set<ETokenType> IGNORED_BRACES_PREDECESSORS = EnumSet
			.of(
			// this is to avoid reporting
			// "new Object[]{}" and "String[] arr = {}" in Java
			ETokenType.RBRACK, ETokenType.EQ,
					// this is to avoid object constructors in JavaScript
					ETokenType.LPAREN, ETokenType.COMMA, ETokenType.COLON,
					ETokenType.OROR, ETokenType.OR);

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "empty-methods", attribute = "allow", optional = true, description = ""
			+ "If this is set to true, empty methods (which are technically empty blocks as well) are allowed. "
			+ "Default is false.")
	public boolean allowEmptyMethods = false;

	/** {@inheritDoc} */
	@Override
	protected void analyzeTokens(List<IToken> tokens, ITokenElement element)
			throws ConQATException {
		Map<Integer, ShallowEntity> closingEntities = determineClosingEntities(element);

		IToken previousToken = null;
		for (IToken token : tokens) {
			if (previousToken == null) {
				previousToken = token;
				continue;
			}

			if (previousToken.getType() == ETokenType.LBRACE
					&& token.getType() == ETokenType.RBRACE) {
				ShallowEntity entity = closingEntities.get(token.getOffset());
				if (!allowEmptyMethods || entity == null
						|| entity.getType() != EShallowEntityType.METHOD) {
					createFindingForFilteredOffsets(constructMessage(entity),
							element, previousToken.getOffset(),
							token.getEndOffset());
				}
			} else if (IGNORED_BRACES_PREDECESSORS.contains(previousToken
					.getType())) {
				// we ignore both lbrace and comments here, as a comment
				// does not invalidate the meaning of the prefix
				if (token.getType() == ETokenType.LBRACE
						|| token.getType().getTokenClass() == ETokenClass.COMMENT) {
					continue;
				}
			}

			previousToken = token;
		}
	}

	/**
	 * Constructs an empty block message using the given entity (which may be
	 * <em>null</em>).
	 */
	private String constructMessage(ShallowEntity entity) {
		String message = "Empty block";

		if (entity != null) {
			message = message + ": " + entity.getSubtype();
		}

		return message;
	}

	/**
	 * Returns a map from token offsets (which are used to uniquely identify a
	 * token) to the entity that ends with the corresponding token.
	 */
	private Map<Integer, ShallowEntity> determineClosingEntities(
			ITokenElement element) {
		Map<Integer, ShallowEntity> closingEntities = new HashMap<Integer, ShallowEntity>();
		try {
			List<ShallowEntity> entities = ShallowParserFactory.parse(element,
					getLogger());
			for (ShallowEntity entity : ShallowEntityTraversalUtils
					.listAllEntities(entities)) {
				if (!entity.isEmpty()) {
					closingEntities.put(
							CollectionUtils.getLast(entity.includedTokens())
									.getOffset(), entity);
				}
			}
		} catch (ConQATException e) {
			getLogger().warn(
					"Failed to parse " + element.getUniformPath() + ": "
							+ e.getMessage(), e);
		}
		return closingEntities;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingCategoryName() {
		return FindingCategoryNames.GENERAL_CHECKS_CATEGORY;
	}
}
