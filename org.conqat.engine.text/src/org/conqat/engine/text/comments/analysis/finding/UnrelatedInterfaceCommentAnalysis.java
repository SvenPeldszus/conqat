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
import org.conqat.engine.text.comments.Comment;
import org.conqat.lib.commons.string.StringUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49805 $
 * @ConQAT.Rating GREEN Hash: 62D8FB400ECFF59AFCEB53B9A593AC8B
 */
@AConQATProcessor(description = "Processor that finds interface comments that do not relate to the method name.")
public class UnrelatedInterfaceCommentAnalysis extends
		InterfaceCommentCoherenceAnalysisBase {

	/** Name of the findings group for unrelated member comments. */
	public static final String FINDING_GROUP_NAME = "Unrelated Member Comment";

	/** Constructor. */
	public UnrelatedInterfaceCommentAnalysis() {
		super(FINDING_GROUP_NAME);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeCoherence(Comment comment) throws ConQATException {

		Comment commentHeadline = getCommentHeadline(comment, false);
		if (getCoherenceCoefficient(commentHeadline) == 0
				&& isNotEmpty(commentHeadline)) {
			createFinding(comment, FINDING_GROUP_NAME);
		}
	}

	/** Returns true if the given comment is not empty. */
	private boolean isNotEmpty(Comment comment) {
		return comment.getText().replaceAll("\\s+", StringUtils.EMPTY_STRING)
				.length() > 0;
	}
}