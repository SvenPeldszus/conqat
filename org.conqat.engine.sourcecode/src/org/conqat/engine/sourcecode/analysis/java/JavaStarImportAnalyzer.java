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
package org.conqat.engine.sourcecode.analysis.java;

import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.scanner.ETokenType;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47769 $
 * @ConQAT.Rating GREEN Hash: 75AE0754870539D5551FDC5436863446
 */
@AConQATProcessor(description = "Creates findings for star imports.")
public class JavaStarImportAnalyzer extends JavaFindingAnalyzerBase {

	/** name of the group to place findings into. */
	public static final String FINDINGS_GROUP_NAME = "Star imports";

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {
		List<ShallowEntity> metaEntities = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.META);
		for (ShallowEntity entity : metaEntities) {
			if (isImportStatement(entity) && containsStar(entity)
					&& !containsStaticKeyword(entity)) {
				createFindingForEntityStart("Star import", element, entity);
			}
		}
	}

	/** Returns whether this contains the static keyword. */
	private boolean containsStaticKeyword(ShallowEntity entity) {
		return TokenStreamUtils.tokenStreamContains(entity.includedTokens(),
				ETokenType.STATIC);
	}

	/** Returns whether this contains a star. */
	private boolean containsStar(ShallowEntity entity) {
		return TokenStreamUtils.tokenStreamContains(entity.includedTokens(),
				ETokenType.MULT);
	}

	/** Returns whether this is an import. */
	private boolean isImportStatement(ShallowEntity entity) {
		return entity.getSubtype().equalsIgnoreCase(ETokenType.IMPORT.name());
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}

}
