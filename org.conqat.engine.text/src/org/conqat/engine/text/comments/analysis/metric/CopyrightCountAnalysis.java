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

import java.util.List;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.engine.text.comments.analysis.CommentClassificationAnalysisBase;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48891 $
 * @ConQAT.Rating GREEN Hash: AB2721350E38A78513FB4AA5AE153821
 */
@AConQATProcessor(description = "Annotates each element with the number of copyright headers found (0 or 1).")
public class CopyrightCountAnalysis extends CommentClassificationAnalysisBase {

	/** Key for number of copyrights */
	@AConQATKey(description = "Number of Copyrights", type = "java.lang.Integer")
	public static final String KEY_NUM_COPYRIGHTS = "#Copyrights";

	/** Number of copyrights seen during analysis. */
	private int numCopyrights;

	/** {@inheritDoc} */
	@Override
	protected void setUpElementAnalysis() {
		numCopyrights = 0;
	}

	/** {@inheritDoc} */
	@Override
	protected void completeElementAnalysis(List<IToken> tokens,
			ITokenElement element) {
		element.setValue(KEY_NUM_COPYRIGHTS, numCopyrights);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) {
		if (category == ECommentCategory.COPYRIGHT) {
			// Fixed value as this is only a presence check
			numCopyrights = 1;
		}
	}
}