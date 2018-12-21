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
package org.conqat.engine.sourcecode.analysis;

import java.util.List;

import org.conqat.engine.commons.findings.FindingCategoryNames;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51136 $
 * @ConQAT.Rating GREEN Hash: D4D2009659642DBE15BB53743D086EF4
 */
@AConQATProcessor(description = "Creates findings for files that have exactly 0 SLOC")
public class MissingCodeAnalyzer extends TokenElementFindingAnalyzerBase {

	/** The finding's group name */
	public static final String FINDINGS_GROUP_NAME = "Missing Code";

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingCategoryName() {
		return FindingCategoryNames.GENERAL_CHECKS_CATEGORY;
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeTokens(List<IToken> tokens, ITokenElement element)
			throws ConQATException {
		for (IToken token : tokens) {
			if (SLOCAnalyzer.NON_COMMENT_TOKEN_CLASSES.contains(token.getType()
					.getTokenClass()) && !StringUtils.isEmpty(token.getText())) {
				return;
			}
		}

		int filteredLength = element.getTextContent().length();
		int unfilteredLength = element.getUnfilteredTextContent().length();
		if (unfilteredLength > filteredLength) {
			// do not create finding if the file is only empty because of
			// filters
			return;
		}

		createFinding("Element does not contain any actual code.", element);
	}
}