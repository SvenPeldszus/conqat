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

package org.conqat.engine.text.comments.utils;

import java.util.List;
import java.util.regex.Pattern;

import org.conqat.engine.text.comments.classification.CodeRecognizer;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Comment utility methods.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51513 $
 * @ConQAT.Rating GREEN Hash: 72DE86D3914286C5F50687A74B4AAE97
 */
public class CommentUtils {

	/** Pattern to detect java doc tags. */
	private static final Pattern JAVA_DOC_TAG_PATTERN = Pattern
			.compile("@param|@return|@throws|@since|@deprecated|@author|@see|@serial");

	/**
	 * Pattern used to detect default comments in
	 * {@link #isDefaultComment(String)}.
	 */
	private static final Pattern DEFAULT_COMMENT_PATTERN = Pattern.compile(
			"[<@](inheritDoc|deprecated|see)|constructor|@ConQAT\\.Doc",
			Pattern.CASE_INSENSITIVE);

	/**
	 * Returns true if the comment is a inheritDoc comment, a constructor, a
	 * ConQAT Doc comment or deprecated or only refers to another comment with a
	 * see tag.
	 */
	public static boolean isDefaultComment(String comment) {
		return DEFAULT_COMMENT_PATTERN.matcher(comment).find();
	}

	/** Returns true if comment contains no other information than java doc tags */
	public static boolean hasOnlyJavaDoc(String comment) {
		return StringUtils.isEmpty(normalizeComment(comment));
	}

	/**
	 * Normalizes a comment by removing tags, identifiers, java doc elements,
	 * line breaks and _ and }.
	 */
	private static String normalizeComment(String comment) {
		String result = removeJavaDocElements(comment);
		result = removeLineBreaks(result);
		result = result.replaceAll("_", " ");
		result = result.replaceAll("}", StringUtils.EMPTY_STRING);
		return result;
	}

	/** Removes all java doc tags. */
	private static String removeJavaDocElements(String comment) {
		return JAVA_DOC_TAG_PATTERN.matcher(comment).replaceAll(
				StringUtils.EMPTY_STRING);
	}

	/** Removes line breaks. */
	private static String removeLineBreaks(String comment) {
		return StringUtils.replaceLineBreaks(comment, " ");
	}

	/**
	 * Returns the normalized text in comments after removing code snippets.
	 * Please note that the result may be empty if all text in the comment is
	 * recognized as code.
	 */
	public static String getTextInComment(String comment, ELanguage language) {
		StringBuilder result = new StringBuilder();

		List<String> lines = StringUtils.splitLinesAsList(comment);
		for (String line : lines) {
			if (!CodeRecognizer.isCodeLine(line, language)) {
				result.append("\n" + line);
			}
		}
		// we don't normalize until the end of this method as the normalization
		// can influence the code detection.
		return normalizeComment(result.toString());
	}

}
