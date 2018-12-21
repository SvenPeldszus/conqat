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
package org.conqat.engine.text.comments.classification;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.utils.CommentUtils;
import org.conqat.engine.text.identifier.CompoundBreaker;
import org.conqat.engine.text.identifier.EStemmer;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Helper class to calculate the coherence between a comment and a method name
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49806 $
 * @ConQAT.Rating GREEN Hash: E936A3820445862894457482244D7B65
 */
public class CoherenceUtils {

	/**
	 * Returns true if a word in the comment (separated by white space) and the
	 * method name is sufficiently similar. Similarity is determined by edit
	 * distance.
	 */
	private static boolean hasSimilarWord(String methodName, String comment,
			ELanguage language) {
		List<String> words = StringUtils.lowercaseList(getCommentWords(comment,
				language));
		for (String part : getIdentifierNameParts(methodName)) {
			for (String word : words) {
				if (StringUtils.isEditDistanceAtMost1(part, word)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Returns the number of words in the comment (separated by white space)
	 * that are similar to a word in the identifier. Similarity is determined by
	 * edit distance.
	 */
	public static int getNumCorrespondingWords(String identifier,
			String comment, ELanguage language) {

		List<String> words = stemAll(getCommentWords(comment, language));
		List<String> identifierParts = stemAll(getIdentifierNameParts(identifier));

		int count = 0;
		for (String word : words) {
			for (String part : identifierParts) {
				if (StringUtils.isEditDistanceAtMost1(part, word)) {
					count++;
					break;
				}
			}
		}
		return count;
	}

	/**
	 * Returns a list with each word transformed to lower case and then stemmed.
	 */
	private static List<String> stemAll(List<String> words) {
		List<String> result = new ArrayList<>(words.size());
		for (String word : words) {
			result.add(EStemmer.ENGLISH.stem(word.toLowerCase()));
		}
		return result;
	}

	/**
	 * Returns all subsets of words in the identifier. The resulting words will
	 * be lowercased.
	 */
	private static List<String> getIdentifierNameParts(String identifier) {
		List<String> identifierParts = CompoundBreaker
				.breakCompound(identifier);
		List<String> result = new ArrayList<String>();
		for (int start = 0; start < identifierParts.size(); ++start) {
			for (int end = start + 1; end <= identifierParts.size(); ++end) {
				result.add(StringUtils.concat(
						identifierParts.subList(start, end),
						StringUtils.EMPTY_STRING).toLowerCase());
			}
		}
		return result;
	}

	/** Returns the words in the comment. */
	public static List<String> getCommentWords(String comment,
			ELanguage language) {
		comment = CommentUtils.getTextInComment(comment, language);
		List<String> result = new ArrayList<>();
		Matcher matcher = Pattern.compile("[\\p{L}\\p{N}]{2,}")
				.matcher(comment);
		while (matcher.find()) {
			result.add(matcher.group());
		}
		return result;
	}

	/**
	 * Returns true if the comment has a context relation to its method. Returns
	 * true for default comments and comments that only contain java doc.
	 */
	public static boolean hasContextCorrelation(Comment comment) {
		String commentText = comment.getText();
		if (CommentUtils.isDefaultComment(commentText)
				|| CommentUtils.hasOnlyJavaDoc(commentText)) {
			return true;
		}

		return CoherenceUtils.hasSimilarWord(comment.getMethodFinder()
				.getNextDefinition(comment.getTokenIndex()), commentText,
				comment.getLanguage());
	}
}
