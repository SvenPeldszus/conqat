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
package org.conqat.engine.text.comments.analysis;

import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;

/**
 * Interface for comment classifiers that can map instance of {@link Comment} to
 * an {@link ECommentCategory}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47591 $
 * @ConQAT.Rating GREEN Hash: E279C79128F27F67F77ADE9DD975FBB7
 */
public interface ICommentClassifier {

	/** Classifies the given comment */
	ECommentCategory getClassification(Comment comment);
}
