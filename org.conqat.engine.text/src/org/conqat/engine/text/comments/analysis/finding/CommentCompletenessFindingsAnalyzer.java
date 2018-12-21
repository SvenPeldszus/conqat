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

import org.conqat.engine.commons.findings.FindingCategoryNames;
import org.conqat.engine.commons.findings.FindingGroup;
import org.conqat.engine.commons.findings.FindingReport;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.util.ResourceUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.text.comments.analysis.CommentCompletenessAnalyzerBase;
import org.conqat.lib.commons.collections.CollectionUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47189 $
 * @ConQAT.Rating GREEN Hash: CA7930BB6DFD4DF8AAB5C5FB11BE6888
 */
@AConQATProcessor(description = "Checks for selected shallow entities whether they are commented and creates findings if not.")
public class CommentCompletenessFindingsAnalyzer extends
		CommentCompletenessAnalyzerBase {

	/** Name of the findings group. */
	public static final String FINDING_GROUP_NAME = "Missing Interface Comment";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The default key used for storing the findings.", type = "java.lang.List<Finding>")
	public static final String DEFAULT_FINDINGS_KEY = "findings";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "category-name", attribute = "value", optional = true, description = ""
			+ "The name of the finding category, default is '"
			+ FindingCategoryNames.COMMENTS_CATEGORY + "'.")
	public String categoryName = FindingCategoryNames.COMMENTS_CATEGORY;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "findings-key", attribute = "key", optional = true, description = ""
			+ "The key used for storing the findings, default is '"
			+ DEFAULT_FINDINGS_KEY + "'")
	public String findingsKey = DEFAULT_FINDINGS_KEY;

	/** The finding report. */
	private FindingReport findingReport;

	/** The findings group used. */
	private FindingGroup findingGroup;

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITokenResource root) throws ConQATException {
		super.setUp(root);
		findingReport = NodeUtils.getFindingReport(root);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeSelectedEntity(ShallowEntity entity,
			ITokenElement element, boolean isCommented) throws ConQATException {
		if (!isCommented) {
			int endOffset = CollectionUtils.getLast(entity.ownStartTokens())
					.getEndOffset();
			ResourceUtils.createAndAttachFindingForFilteredRegion(
					getFindingsGroup(), "Interface comment missing", element,
					entity.getStartOffset(), endOffset, findingsKey);
		}
	}

	/** Returns the findings group used. */
	private FindingGroup getFindingsGroup() {
		if (findingGroup == null) {
			findingGroup = findingReport.getOrCreateCategory(categoryName)
					.getOrCreateFindingGroup(FINDING_GROUP_NAME);
		}

		return findingGroup;
	}

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] { findingsKey };
	}
}
