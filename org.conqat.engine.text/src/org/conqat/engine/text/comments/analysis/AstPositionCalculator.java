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
package org.conqat.engine.text.comments.analysis;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.scanner.IToken;

/**
 * Helper class to determine the logical AST position of a comment. This decides
 * whether this is at the position for header, interface, or inline comments.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49801 $
 * @ConQAT.Rating GREEN Hash: 5976BCF929A725B6D398538C7604FC8C
 */
public class AstPositionCalculator {

	/**
	 * Returns the logical position of the comment in the AST, represented as a
	 * number. The comment is represented by its token position in the given
	 * token list.
	 * 
	 * @param commentToken
	 *            the comment token for which the AST position is calculated
	 * @param topLevelEntities
	 *            shallow parsed top-level entities for the tokens
	 */
	public static EAstPosition getAstPosition(IToken commentToken,
			List<ShallowEntity> topLevelEntities) {

		if (!hasNonMetaEntities(topLevelEntities)) {
			// this is e.g. a file that is completely commented out.
			return EAstPosition.HEADER;
		}

		return traverseEntities(topLevelEntities, commentToken);
	}

	/**
	 * Returns whether the given list of entities contains any non-meta
	 * entities.
	 */
	private static boolean hasNonMetaEntities(List<ShallowEntity> entities) {
		for (ShallowEntity entity : entities) {
			if (entity.getType() != EShallowEntityType.META) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Traverses all entities and returns the AST position for the given comment
	 * token.
	 */
	private static EAstPosition traverseEntities(List<ShallowEntity> entities,
			IToken commentToken) {
		ShallowEntity previous = null;
		for (ShallowEntity entity : entities) {
			if (entity.getType() == EShallowEntityType.META) {
				// we skip meta entities, as they are not target of comments
				continue;
			}

			if (commentToken.getEndOffset() < entity.getStartOffset()) {
				return determineLocationForReferenceEntity(entity);
			}
			if (commentToken.getEndOffset() < entity.getEndOffset()) {
				if (hasNonMetaEntities(entity.getChildren())) {
					return traverseEntities(entity.getChildren(), commentToken);
				}
				// comments in leaf entities are always inline, as they are
				// either in statements or in locations where no interface
				// comments are expected (e.g. between the class keyword and
				// the class name).
				return EAstPosition.INLINE;
			}
			previous = entity;
		}

		CCSMAssert
				.isTrue(previous != null,
						"May not call with empty entities list or entities list containing only meta entities.");
		return determineLocationForReferenceEntity(previous);
	}

	/**
	 * Handles the case that the comment token is between two entities of the
	 * AST. Each of the entities may be null to indicate that the comment is at
	 * the begin or end of the file. But not both of the entities may be null.
	 */
	private static EAstPosition determineLocationForReferenceEntity(
			ShallowEntity referenceEntity) {
		switch (referenceEntity.getType()) {
		case METHOD:
			if (isPropertyGetterSetter(referenceEntity)) {
				return EAstPosition.INLINE;
			}
			// fall-through intended
		case ATTRIBUTE:
			return EAstPosition.INTERFACE;
		case STATEMENT:
			return EAstPosition.INLINE;
		case MODULE:
		case TYPE:
			return EAstPosition.HEADER;
		case META:
			throw new AssertionError(
					"May not call with META entity as reference!");
		default:
			throw new AssertionError("Unknown entity type: "
					+ referenceEntity.getType());
		}
	}

	/** Returns whether the given entity is a get/set method within a property. */
	private static boolean isPropertyGetterSetter(ShallowEntity entity) {
		return entity.getParent() != null
				&& entity.getParent().getType() == EShallowEntityType.ATTRIBUTE;
	}

	/** The possible positions w.r.t. the AST. */
	public static enum EAstPosition {
		/** Header and copyright comments */
		HEADER,

		/** Interface comments */
		INTERFACE,

		/** Inline comments */
		INLINE;
	}
}
