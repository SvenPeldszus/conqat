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
package org.conqat.engine.text.comments.utils;

import java.util.regex.Pattern;

import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.lib.commons.enums.EnumUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Utility code for dealing with comment tags. Tags are used to mark the
 * expected classification result of a comment, both during learning and
 * testing. Tags are included as part of the comment and separated using the
 * {@link #SEPARATOR}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49815 $
 * @ConQAT.Rating GREEN Hash: 9E4F796BFB9C91834F12323C0D4DDA19
 */
public class CommentTaggingUtils {

	/**
	 * Separator tag, comments are tagged like $copyright$ (but using the
	 * section sign instead of the dollar sign).
	 */
	private static final String SEPARATOR = "\u00A7";

	/** Pattern to detect tags in a comment . */
	private static final Pattern TAG_PATTERN = Pattern.compile(SEPARATOR + ".*"
			+ SEPARATOR);

	/** Returns the annotated comment category or null. */
	public static ECommentCategory getAnnotatedCategory(Comment comment) {
		return getCommentCategoryTag(comment.getRawCommentText());
	}

	/**
	 * Returns the comment category corresponding to the classification tag of a
	 * comment, e.g."header" or "interface" or null if the tag could not be
	 * matched. This expects the comment category tag to be the very first tag
	 * if multiple (comma separated) tags are present.
	 */
	private static ECommentCategory getCommentCategoryTag(String comment) {
		String[] tags = getCommentTags(comment).split(",");
		for (String tag : tags) {
			ECommentCategory value = EnumUtils.valueOfIgnoreCase(
					ECommentCategory.class, tag);
			if (value != null) {
				return value;
			}
		}
		return null;
	}

	/**
	 * Removes all tags that start and end with {@link #SEPARATOR} in the given
	 * comment.
	 */
	public static String removeTags(String comment) {
		return TAG_PATTERN.matcher(comment)
				.replaceAll(StringUtils.EMPTY_STRING);
	}

	/**
	 * Returns the tags of a comment. Assumes that comment are tagged like $tag$
	 * or $tag1,tag2$ ($ should be actually the section sign). This is used for
	 * reading the machine learning training data. Multiple tags can be used
	 * when the same training set is tagged for different learning goals.
	 * Returns an empty string if no tag was found.
	 * 
	 * @return the classification tag as a comma separated string.
	 */
	private static String getCommentTags(String comment) {
		String[] parts = comment.split(SEPARATOR);
		if (parts.length != 3) {
			return StringUtils.EMPTY_STRING;
		}
		return parts[1];
	}
}
