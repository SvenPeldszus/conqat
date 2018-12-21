/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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
package org.conqat.engine.code_clones.normalization.token.configuration;

import java.io.Serializable;

import org.conqat.engine.text.identifier.EStemmer;
import org.conqat.engine.text.identifier.EStopWords;

/**
 * Configuration for token-based normalization.
 * <p>
 * Determines, how normalization is performed.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49199 $
 * @ConQAT.Rating GREEN Hash: EBD864313614214E22083720AF9B976F
 */
public interface ITokenConfiguration extends Serializable {

	/** Get name of this token configuration. */
	String getName();

	/** Returns whether to ignore end of statement tokens. */
	boolean isIgnoreEndOfStatementTokens();

	/** Returns whether to ignore delimiters. */
	boolean isIgnoreDelimiters();

	/** Returns whether to ignore comments. */
	boolean isIgnoreComments();

	/** Returns whether to ignore pre-processor directives. */
	boolean isIgnorePreprocessorDirectives();

	/** Returns whether to normalize identifiers. */
	boolean isNormalizeIdentifiers();

	/** Returns whether to normalize fully qualified names. */
	boolean isNormalizeFullyQualifiedNames();

	/** Returns whether to normalize type keywords. */
	boolean isNormalizeTypeKeywords();

	/** Returns whether to normalize string literals. */
	boolean isNormalizeStringLiterals();

	/** Returns whether to normalize char literals. */
	boolean isNormalizeCharacterLiterals();

	/** Returns whether to normalize number literals. */
	boolean isNormalizeNumberLiterals();

	/** Returns whether to normalize boolean literals. */
	boolean isNormalizeBooleanLiterals();

	/** Returns whether to ignore the "this" reference. */
	boolean isIgnoreThis();

	/** Returns whether to ignore visibility modifiers. */
	boolean isIgnoreModifier();

	/** Returns whether to perform word stemming. */
	boolean isStemWords();

	/**
	 * Retrieves stemmer. If no stemmer is set, <code>null</code> is returned.
	 * Must not return <code>null</code>, if stemming is enabled.
	 */
	EStemmer getStemmer();

	/** Returns whether to ignore stop words. */
	boolean isIgnoreStopWords();

	/**
	 * Retrieves the set of stop words. If stop words are not ignored, null is
	 * returned. Must not return <code>null</code>, if stop word elimination is
	 * enabled.
	 */
	EStopWords getStopWords();

}