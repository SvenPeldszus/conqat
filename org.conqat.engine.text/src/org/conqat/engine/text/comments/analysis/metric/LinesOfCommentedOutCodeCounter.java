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
package org.conqat.engine.text.comments.analysis.metric;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Processor to count lines of code that are commented out based on machine
 * learning.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49770 $
 * @ConQAT.Rating GREEN Hash: D6DC668C134950A20559CC5B46111B8D
 */
@AConQATProcessor(description = "Counts the lines of code that are commented out based on machine learning.")
public class LinesOfCommentedOutCodeCounter extends CommentCountBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Commented Out Code Count", type = "java.lang.Integer")
	public static final String KEY_COMMENTED_OUT_CODE_LOC = "commented out code (LOC)";

	/** {@inheritDoc} */
	@Override
	protected int count(IElement element, Comment comment,
			ECommentCategory category) {
		if (category == ECommentCategory.CODE) {
			return StringUtils.countLines(comment.getText());
		}
		return 0;
	}

	/** {@inheritDoc} */
	@Override
	protected String getKey() {
		return KEY_COMMENTED_OUT_CODE_LOC;
	}
}