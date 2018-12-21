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
package org.conqat.engine.sourcecode.coverage.volume;

import java.util.EnumMap;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * This class manages the statement subtypes for statements that hold a
 * condition.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51020 $
 * @ConQAT.Rating GREEN Hash: 1835ABC50072F520B8BD2ED7C6694709
 */
public class ConditionalStatementSubtypes {

	/** Mapping from languages to their conditional subtypes. */
	private static final Map<ELanguage, Set<String>> CONDITIONAL_SUBTYPES = new EnumMap<>(
			ELanguage.class);

	static {
		Set<String> clikeConditionals = CollectionUtils.asHashSet(
				SubTypeNames.IF, SubTypeNames.ELSE_IF, SubTypeNames.WHILE,
				SubTypeNames.DO, SubTypeNames.FOR, SubTypeNames.FOREACH);
		CONDITIONAL_SUBTYPES.put(ELanguage.JAVA, clikeConditionals);
		CONDITIONAL_SUBTYPES.put(ELanguage.CPP, clikeConditionals);
		CONDITIONAL_SUBTYPES.put(ELanguage.CS, clikeConditionals);

		// loop is explicitly excluded for Ada, as it loop unconditionally
		CONDITIONAL_SUBTYPES.put(ELanguage.ADA, CollectionUtils.asHashSet(
				SubTypeNames.IF, SubTypeNames.ELSIF, SubTypeNames.FOR,
				SubTypeNames.WHILE));
	}

	/** Returns whether the given language is supported by this class. */
	public static boolean supportsLanguage(ELanguage language) {
		return CONDITIONAL_SUBTYPES.containsKey(language);
	}

	/**
	 * Returns whether the given entity represents a conditional statement. This
	 * includes not only if-statements but also loops with conditions.
	 */
	public static boolean isConditionalStatement(ShallowEntity entity) {
		if (entity.isEmpty()
				|| entity.getType() != EShallowEntityType.STATEMENT) {
			return false;
		}

		ELanguage language = CollectionUtils.getAny(entity.includedTokens())
				.getLanguage();
		Set<String> subtypes = CONDITIONAL_SUBTYPES.get(language);

		String subtype = entity.getSubtype();
		if (!language.isCaseSensitive()) {
			subtype = subtype.toLowerCase();
		}

		return subtypes != null && subtypes.contains(subtype);
	}
}
