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
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;

/**
 * Factory for creating {@link IConditionExtractor}s for different languages.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51131 $
 * @ConQAT.Rating GREEN Hash: 2F935C73BF9EE215047B1B78284D94C5
 */
public class ConditionParserFactory {

	/**
	 * Mapping from languages to the corresponding {@link IConditionExtractor}s.
	 */
	private static final Map<ELanguage, IConditionExtractor> CONDITION_EXTRACTORS = new EnumMap<>(
			ELanguage.class);

	/**
	 * Mapping from languages to the corresponding {@link ISubConditionParser}s.
	 */
	private static final Map<ELanguage, ISubConditionParser> SUB_CONDITION_PARSERS = new EnumMap<>(
			ELanguage.class);

	static {
		registerLanguage(
				ELanguage.JAVA,
				new CLikeConditionExtractor(ELanguage.JAVA),
				new SplitSubConditionParser(EnumSet.of(ETokenType.ANDAND,
						ETokenType.OROR), EnumSet.of(ETokenType.AND,
						ETokenType.OR)));
		registerLanguage(
				ELanguage.CPP,
				new CLikeConditionExtractor(ELanguage.CPP),
				new SplitSubConditionParser(EnumSet.of(ETokenType.ANDAND,
						ETokenType.OROR)));
		registerLanguage(
				ELanguage.CS,
				new CLikeConditionExtractor(ELanguage.CS),
				new SplitSubConditionParser(EnumSet.of(ETokenType.ANDAND,
						ETokenType.OROR)));

		registerLanguage(
				ELanguage.ADA,
				new AdaConditionExtractor(),
				new SplitSubConditionParser(EnumSet.of(ETokenType.AND,
						ETokenType.OR, ETokenType.XOR)));
	}

	/** Registers a language with the factory. */
	private static void registerLanguage(ELanguage language,
			IConditionExtractor conditionExtractor,
			ISubConditionParser subConditionParser) {
		CONDITION_EXTRACTORS.put(language, conditionExtractor);
		SUB_CONDITION_PARSERS.put(language, subConditionParser);
	}

	/**
	 * Returns the {@link IConditionExtractor} for the given language.
	 * 
	 * @throws ConQATException
	 *             if no {@link IConditionExtractor} is defined for the given
	 *             language
	 */
	public static IConditionExtractor createConditionExtractor(
			ELanguage language) throws ConQATException {
		IConditionExtractor extractor = CONDITION_EXTRACTORS.get(language);
		if (extractor == null) {
			throw new ConQATException("No condition extractor is defined for "
					+ language.getReadableName());
		}
		return extractor;
	}

	/**
	 * Returns the {@link ISubConditionParser} for the given language.
	 * 
	 * @throws ConQATException
	 *             if no {@link ISubConditionParser} is defined for the given
	 *             language
	 */
	public static ISubConditionParser createSubConditionParser(
			ELanguage language) throws ConQATException {
		ISubConditionParser parser = SUB_CONDITION_PARSERS.get(language);
		if (parser == null) {
			throw new ConQATException("No sub condition parser is defined for "
					+ language.getReadableName());
		}
		return parser;
	}
}
