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

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49762 $
 * @ConQAT.Rating GREEN Hash: 35A2C2D3AB1405597116EDEFD0090C51
 */
@AConQATProcessor(description = "Creates findings for commented out code.")
public class CommentedOutCodeAnalysis extends CommentFindingAnalysisBase {

	/** Name of the findings group for commented out code. */
	public static final String FINDING_GROUP_NAME = "Commented Out Code";

	/** Start tag for a GWT comment. */
	private static final String GWT_NATIVE_CODE_START = "/*-{";

	/** End tag for a GWT comment. */
	private static final String GWT_NATIVE_CODE_END = "}-*/";

	/** Constructor */
	public CommentedOutCodeAnalysis() {
		super(FINDING_GROUP_NAME);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) throws ConQATException {
		if (category != ECommentCategory.CODE) {
			return;
		}

		if (comment.getLanguage() == ELanguage.JAVA && isGwtNativeCode(comment)) {
			return;
		}

		createFinding(comment, FINDING_GROUP_NAME);
	}

	/**
	 * Returns whether the given comment looks like GWT native code
	 * (http://www.gwtproject.org/doc/latest/DevGuideCodingBasicsJSNI.html).
	 */
	private boolean isGwtNativeCode(Comment comment) {
		String commentText = comment.getRawCommentText().trim();
		return commentText.startsWith(GWT_NATIVE_CODE_START)
				&& commentText.endsWith(GWT_NATIVE_CODE_END);
	}
}