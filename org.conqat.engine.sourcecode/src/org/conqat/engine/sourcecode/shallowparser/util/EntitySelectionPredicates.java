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
package org.conqat.engine.sourcecode.shallowparser.util;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.predicate.IPredicate;
import org.conqat.lib.commons.predicate.OrPredicate;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Defines valid primitive expressions that can be used in the
 * {@link EntitySelectionExpressionParser}. Each expression is defined by a
 * public static method of same name. Methods found here must be parameterless
 * or accept a single string parameter and must return an {@link IPredicate}
 * over {@link ShallowEntity}. To avoid clashes with Java keywords, an optional
 * prefix "select" may be used.
 * 
 * Note that the comments for the public methods describe not exactly the
 * function, but rather the returned predicate.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51416 $
 * @ConQAT.Rating GREEN Hash: CC900E7F122AC56E17383AF07D4E9C15
 */
public class EntitySelectionPredicates {

	/** Selects all modules/namespaces. */
	public static IPredicate<ShallowEntity> module() {
		return typePredicate(EShallowEntityType.MODULE);
	}

	/** Selects all types/classes. */
	public static IPredicate<ShallowEntity> type() {
		return typePredicate(EShallowEntityType.TYPE);
	}

	/** Selects all methods/functions. */
	public static IPredicate<ShallowEntity> method() {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				// we have to filter methods with an attribute as parent, as
				// these are get/set methods of a property, which are usually
				// not seens as methods.
				return entity.getType() == EShallowEntityType.METHOD
						&& (entity.getParent() == null || entity.getParent()
								.getType() != EShallowEntityType.ATTRIBUTE);
			}
		};
	}

	/** Selects all attributes. */
	public static IPredicate<ShallowEntity> attribute() {
		return typePredicate(EShallowEntityType.ATTRIBUTE);
	}

	/**
	 * Selects all properties, defined attributes that have only method
	 * children.
	 */
	public static IPredicate<ShallowEntity> property() {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				if (entity.getType() != EShallowEntityType.ATTRIBUTE) {
					return false;
				}

				for (ShallowEntity child : entity.getChildren()) {
					if (child.getType() != EShallowEntityType.METHOD) {
						return false;
					}
				}

				return !entity.getChildren().isEmpty();
			}
		};
	}

	/** Selects all statements. */
	public static IPredicate<ShallowEntity> statement() {
		return typePredicate(EShallowEntityType.STATEMENT);
	}

	/** Selects all meta information (defines, annotations, etc.). */
	public static IPredicate<ShallowEntity> meta() {
		return typePredicate(EShallowEntityType.META);
	}

	/** Creates a predicate for checking the type of an entity. */
	private static IPredicate<ShallowEntity> typePredicate(
			final EShallowEntityType type) {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				return entity.getType() == type;
			}
		};
	}

	/** Matches all public entitys. */
	public static IPredicate<ShallowEntity> selectPublic() {
		return modifierPredicate(ETokenType.PUBLIC);
	}

	/** Matches all protected entitys. */
	public static IPredicate<ShallowEntity> selectProtected() {
		return modifierPredicate(ETokenType.PROTECTED);
	}

	/** Matches all entitys marked with override. */
	public static IPredicate<ShallowEntity> selectOverride() {
		return modifierPredicate(ETokenType.OVERRIDE);
	}

	/** Matches all private entitys. */
	public static IPredicate<ShallowEntity> selectPrivate() {
		return modifierPredicate(ETokenType.PRIVATE);
	}

	/** Matches all entitys with default visibility. */
	public static IPredicate<ShallowEntity> selectDefault() {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity element) {
				List<IToken> tokens = element.ownStartTokens();
				return !TokenStreamUtils.containsAny(tokens, 0, tokens.size(),
						ETokenType.PUBLIC, ETokenType.PROTECTED,
						ETokenType.PRIVATE);
			}
		};
	}

	/** Matches all final entitys. */
	public static IPredicate<ShallowEntity> selectFinal() {
		return modifierPredicate(ETokenType.FINAL);
	}

	/** Matches all static entitys. */
	public static IPredicate<ShallowEntity> selectStatic() {
		return modifierPredicate(ETokenType.STATIC);
	}

	/** Returns a predicate that checks for existence of the given modifier. */
	private static IPredicate<ShallowEntity> modifierPredicate(
			final ETokenType modifier) {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity element) {
				return TokenStreamUtils
						.find(element.ownStartTokens(), modifier) != TokenStreamUtils.NOT_FOUND;
			}
		};
	}

	/** Matches all primitive entities (i.e. those without children). */
	public static IPredicate<ShallowEntity> primitive() {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				return entity.getChildren().isEmpty();
			}
		};
	}

	/** Matches entities by their subtype. */
	public static IPredicate<ShallowEntity> subtype(final String subtype) {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				return subtype.equals(entity.getSubtype());
			}
		};
	}

	/** Matches entities by their name. */
	public static IPredicate<ShallowEntity> name(final String name) {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				return name.equals(entity.getName());
			}
		};
	}

	/**
	 * Matches all entities that are annotated with an annotation of given name
	 * (excluding the '@' sign).
	 */
	public static IPredicate<ShallowEntity> annotated(String annotationName) {
		final String normalizedAnnotationName = StringUtils.stripPrefix(
				annotationName, "@");
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				// annotations must be part of same parent
				ShallowEntity parent = entity.getParent();
				if (parent == null) {
					return false;
				}

				List<ShallowEntity> children = parent.getChildren();
				int index = children.indexOf(entity);
				index -= 1;
				while (index >= 0
						&& children.get(index).getType() == EShallowEntityType.META) {
					ShallowEntity metaEntity = children.get(index);
					if ("annotation".equals(metaEntity.getSubtype())
							&& normalizedAnnotationName
									.equalsIgnoreCase(metaEntity.getName())) {
						return true;
					}
					index -= 1;
				}
				return false;
			}
		};
	}

	/**
	 * Matches simple getters, i.e. methods starting with "get" or "is" and with
	 * at most one contained statement.
	 */
	@SuppressWarnings("unchecked")
	public static IPredicate<ShallowEntity> simpleGetter() {
		return OrPredicate.create(simpleMethod("get"), simpleMethod("is"));
	}

	/**
	 * Matches simple setters, i.e. methods starting with "set" and with at most
	 * one contained statement.
	 */
	public static IPredicate<ShallowEntity> simpleSetter() {
		return simpleMethod("set");
	}

	/**
	 * Matches simple methods with the given prefix. A method is simple, if it
	 * contains as most one statement.
	 */
	public static IPredicate<ShallowEntity> simpleMethod(final String namePrefix) {
		return new IPredicate<ShallowEntity>() {
			@Override
			public boolean isContained(ShallowEntity entity) {
				boolean isSimpleMethod = entity.getType() == EShallowEntityType.METHOD
						&& getStatementCount(entity) <= 1;
				if (!isSimpleMethod) {
					return false;
				}

				if (entity.getName() == null) {
					return StringUtils.isEmpty(namePrefix);
				}
				return entity.getName().startsWith(namePrefix);
			}

			/**
			 * Returns the number of statements contained as children of the
			 * entity.
			 */
			private int getStatementCount(ShallowEntity entity) {
				return ShallowEntityTraversalUtils.listEntitiesOfType(
						entity.getChildren(), EShallowEntityType.STATEMENT)
						.size();
			}
		};
	}
}
