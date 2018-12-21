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
package org.conqat.engine.text.comments.analysis.finding;

import java.util.EnumSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;

/**
 * {@ConQAT.Doc}
 * 
 * Processor to analyze the usefulness of comments by looking for exclamation
 * and question marks.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49803 $
 * @ConQAT.Rating GREEN Hash: 05459A0F4AEF4020BFFFFBA1DD6D65B9
 */
@AConQATProcessor(description = "Calculates the exclamation and question mark heuristic to evaluate the usefulness of comments.")
public class ExclamationQuestionMarkAnalysis extends CommentFindingAnalysisBase {

	/**
	 * Name of the findings group for comments containing exclamation and
	 * question marks.
	 */
	public static final String FINDING_GROUP_NAME = "Exclamation or Question Mark in Comment";

	/** Pattern that a comment must match to count as finding. */
	private static final Pattern MATCH_PATTERN = Pattern.compile("[!?]");

	/**
	 * Pattern that identifies false positives. We skip comments containing at
	 * least two quotes, URLs (://), or comparison (!=).
	 */
	private static final Pattern EXCLUDE_PATTERN = Pattern
			.compile("(['\"].*['\"])|://|!=");

	/** The categories supported by this analysis. */
	private static final Set<ECommentCategory> SUPPORTED_CATEGORIES = EnumSet
			.of(ECommentCategory.INLINE, ECommentCategory.INTERFACE,
					ECommentCategory.HEADER);

	/** Constructor. */
	public ExclamationQuestionMarkAnalysis() {
		super(FINDING_GROUP_NAME);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) throws ConQATException {
		if (SUPPORTED_CATEGORIES.contains(category)) {
			analyzeComment(comment);
		}
	}

	/**
	 * Creates findings for a comment that contains a question mark or
	 * exclamation mark.
	 */
	private void analyzeComment(Comment comment) throws ConQATException {
		String commentString = comment.getText();
		if (MATCH_PATTERN.matcher(commentString).find()
				&& !EXCLUDE_PATTERN.matcher(commentString).find()) {
			createFinding(comment, FINDING_GROUP_NAME);
		}
	}
}