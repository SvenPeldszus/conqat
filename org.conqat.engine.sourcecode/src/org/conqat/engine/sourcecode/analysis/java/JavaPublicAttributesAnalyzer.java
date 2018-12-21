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

import java.util.HashSet;
import java.util.List;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51308 $
 * @ConQAT.Rating GREEN Hash: 63FEBFEBAEB783C89E3E991498122AC4
 */
@AConQATProcessor(description = "Checks for public (non constant) attributes in classes.")
public class JavaPublicAttributesAnalyzer extends JavaFindingAnalyzerBase {

	/** Name of the group to put findings into. */
	public static final String FINDINGS_GROUP_NAME = "Public attributes";

	/** List of annotations that should be ignored for this analysis */
	private final HashSet<String> filterAnnotationNames = new HashSet<>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "annotations", description = "Attributes that have annotations with the given names will be ignored for this analysis.")
	public void setIgnoredAnnotations(
			@AConQATAttribute(name = "ignore", description = "An annotation name to ignore.") String annotation) {
		filterAnnotationNames.add(StringUtils.stripPrefix(annotation.trim(),
				"@"));
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {

		for (ShallowEntity entity : ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.ATTRIBUTE)) {

			List<IToken> tokens = entity.includedTokens();
			int identifier = TokenStreamUtils.find(tokens,
					ETokenType.IDENTIFIER);

			CCSMAssert.isTrue(identifier != TokenStreamUtils.NOT_FOUND,
					"Attribute without identifier?");

			boolean isPublic = TokenStreamUtils.tokenStreamContains(tokens, 0,
					identifier, ETokenType.PUBLIC);
			boolean isStatic = TokenStreamUtils.tokenStreamContains(tokens, 0,
					identifier, ETokenType.STATIC);
			boolean isFinal = TokenStreamUtils.tokenStreamContains(tokens, 0,
					identifier, ETokenType.FINAL);

			if (isPublic && !(isStatic && isFinal)
					&& !isFilteredByAnnotation(entity)) {
				createFindingForEntityStart("Non-constant public attribute",
						element, entity);
			}
		}
	}

	/**
	 * Returns whether the given entity has a ConQAT annotation.
	 */
	private boolean isFilteredByAnnotation(ShallowEntity entity) {
		ShallowEntity parent = entity.getParent();
		if (parent == null) {
			return false;
		}

		UnmodifiableList<ShallowEntity> entities = parent.getChildren();

		for (int i = entities.indexOf(entity) - 1; i >= 0; i--) {
			ShallowEntity previousEntity = entities.get(i);
			if (!(previousEntity.getType() == EShallowEntityType.META && previousEntity
					.getSubtype().equals(SubTypeNames.ANNOTATION))) {
				break;
			}
			if (filterAnnotationNames.contains(previousEntity.getName())) {
				return true;
			}
		}
		return false;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}

}
