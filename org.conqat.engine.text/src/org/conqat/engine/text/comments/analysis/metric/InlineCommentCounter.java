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
package org.conqat.engine.text.comments.analysis.metric;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;

/**
 * Processor to count the number of inline comments.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47824 $
 * @ConQAT.Rating GREEN Hash: 88FE32784B3982A0E456ECDABFF73948
 */
@AConQATProcessor(description = "Counts the number of inline comments.")
public class InlineCommentCounter extends CommentCountBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Number of Long Comments", type = "java.lang.Integer")
	public static final String KEY_NUM_INLINE_COMMENTS = "Number of Inline Comments";

	/** {@inheritDoc} */
	@Override
	protected int count(IElement element, Comment comment,
			ECommentCategory category) {
		if (category == ECommentCategory.INLINE) {
			return 1;
		}
		return 0;
	}

	/** {@inheritDoc} */
	@Override
	protected String getKey() {
		return KEY_NUM_INLINE_COMMENTS;
	}
}
