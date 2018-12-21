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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import java.util.List;
import java.util.Set;

import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.util.ResourceUtils;
import org.conqat.engine.sourcecode.analysis.LongestStatementListAnalyzerBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.IShallowEntityVisitor;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.CollectionUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51365 $
 * @ConQAT.Rating GREEN Hash: 88A86D22067562BF195D39874A3A0520
 */
@AConQATProcessor(description = "Annotates each element with the length of the longest statement list found.")
public class ShallowParsedLongestStatementListAnalyzer extends
		LongestStatementListAnalyzerBase<ITokenResource, ITokenElement> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "count-statements", attribute = "enabled", optional = true, description = ""
			+ "If this is enabled, the number of statements are counted instead of (s)loc.")
	public boolean countStatements = false;

	/** {@inheritDoc} */
	@Override
	protected Class<ITokenElement> getElementClass() {
		return ITokenElement.class;
	}

	/** {@inheritDoc} */
	@Override
	protected void calculateStatementListLocations(ITokenElement element,
			Set<Integer> ignoredLines) {
		try {
			ShallowEntity.traverse(
					ShallowParserFactory.parse(element, getLogger()),
					new StatementListLengthVisitior(ignoredLines));
		} catch (ConQATException e) {
			getLogger().warn(
					"Ignoring element " + element.getLocation() + ": "
							+ e.getMessage());
		}
	}

	/** Visitor for calculating maximal statement list length. */
	private final class StatementListLengthVisitior implements
			IShallowEntityVisitor {

		/** The set of ignored lines (lines are 1-based here). */
		private final Set<Integer> ignoredLines;

		/** Constructor. */
		public StatementListLengthVisitior(Set<Integer> ignoredLines) {
			this.ignoredLines = ignoredLines;
		}

		/** {@inheritDoc} */
		@Override
		public boolean visit(ShallowEntity entity) {

			if (entity.getType() == EShallowEntityType.STATEMENT) {
				// we are not interested in sub statements, but we might
				// encounter another method/type within
				return true;
			}

			if (entity.getType() == EShallowEntityType.MODULE) {
				// we do no count code directly in modules, only in methods and
				// classes
				return true;
			}

			List<ShallowEntity> statements = entity
					.getChildrenOfType(EShallowEntityType.STATEMENT);
			if (statements.isEmpty()) {
				return true;
			}

			int startOffset = statements.get(0).getStartOffset();
			int endOffset = CollectionUtils.getLast(statements).getEndOffset();

			try {
				if (countStatements) {
					reportStatementCount(statements, startOffset, endOffset);
				} else {
					reportStatementListForOffsets(startOffset, endOffset,
							ignoredLines);
				}
			} catch (ConQATException e) {
				getLogger().error(
						"Offset conversion failed: " + e.getMessage(), e);
			}
			return true;
		}

		/**
		 * Counts entities of type {@link EShallowEntityType#STATEMENT} under
		 * the given entity
		 */
		private void reportStatementCount(List<ShallowEntity> statements,
				int startOffset, int endOffset) throws ConQATException {
			// We need all childern, including subchildren to make sure we count
			// all statements under the given entity.
			List<ShallowEntity> allStatements = ShallowEntityTraversalUtils
					.listEntitiesOfType(statements,
							EShallowEntityType.STATEMENT);

			TextRegionLocation textRegion = ResourceUtils
					.createTextRegionLocationForFilteredOffsets(currentElement,
							startOffset, endOffset);
			reportMetricValue(allStatements.size(), textRegion);
		}

		/** {@inheritDoc} */
		@Override
		public void endVisit(ShallowEntity entity) {
			// not needed
		}
	}
}