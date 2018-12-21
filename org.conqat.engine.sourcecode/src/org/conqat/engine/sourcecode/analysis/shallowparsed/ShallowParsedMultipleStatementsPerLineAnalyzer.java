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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.commons.findings.FindingCategoryNames;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47751 $
 * @ConQAT.Rating GREEN Hash: 5D8CD1E0C86FDD9D42714F650A7C8580
 */
@AConQATProcessor(description = "This processor produces findings if a line contains more than one statement.")
public class ShallowParsedMultipleStatementsPerLineAnalyzer extends
		ShallowParsedFindingAnalyzerBase {

	/** Name of the findings group. */
	public static final String FINDING_GROUP_NAME = "Multiple statements per line";

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {

		Set<Integer> reportedLines = new HashSet<Integer>();

		List<ShallowEntity> primitiveStatements = ShallowParsingUtils
				.listPrimitiveStatements(entities);
		for (int i = 1; i < primitiveStatements.size(); ++i) {
			ShallowEntity statement1 = primitiveStatements.get(i - 1);
			ShallowEntity statement2 = primitiveStatements.get(i);

			if (statement2.getStartLine() <= statement1.getEndLine()
					&& !statement1.isContinued()) {
				int lineNumber = statement2.getStartLine();
				if (reportedLines.add(lineNumber)) {
					createFindingForFilteredLine(
							"Multiple statements in single line.", element,
							lineNumber);
				}
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDING_GROUP_NAME;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingCategoryName() {
		return FindingCategoryNames.FORMATTING_CATEGORY;
	}
}
