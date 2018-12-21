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
package org.conqat.engine.text.comments.analysis.finding;

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.text.comments.analysis.CommentCompletenessAnalyzerBase;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50378 $
 * @ConQAT.Rating GREEN Hash: 69777C4258FD8EF04A8A7432C848257A
 */
@AConQATProcessor(description = "Creates an assessment for interface comments. The assessment reflects the number of commented (GREEN)"
		+ " and non-commented (RED) interface elements.")
public class CommentCompletenessAssessor extends
		CommentCompletenessAnalyzerBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The default key used for storing the assessment.", type = "org.conqat.lib.commons.assessment.Assessment")
	public static final String DEFAULT_KEY = "comment-completeness";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "write-key", attribute = "value", optional = true, description = ""
			+ "The key used for storing the assessment, default is '"
			+ DEFAULT_KEY + "'")
	public String key = DEFAULT_KEY;

	/** {@inheritDoc} */
	@Override
	protected void analyzeElement(ITokenElement element) throws ConQATException {
		super.analyzeElement(element);
		// we need to create an empty assessment for elements without interface
		NodeUtils.getOrCreateAssessment(element, key);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeSelectedEntity(ShallowEntity entity,
			ITokenElement element, boolean isCommented) {
		Assessment assessment = NodeUtils.getOrCreateAssessment(element, key);
		if (isCommented) {
			assessment.add(ETrafficLightColor.GREEN);
		} else {
			assessment.add(ETrafficLightColor.RED);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] { key };
	}

}
