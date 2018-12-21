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
import java.util.Map;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATPipelineProcessorBase;
import org.conqat.engine.commons.findings.FindingCategory;
import org.conqat.engine.commons.findings.FindingGroup;
import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.commons.findings.util.FindingUtils;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.ResourceUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.ListMap;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49409 $
 * @ConQAT.Rating GREEN Hash: 369554AB159759983B3315A2241E90B9
 */
@AConQATProcessor(description = "This processor analyzes source code "
		+ "for redundant literals (magic numbers). For each redundant literal "
		+ "findings in the affected files are creted. "
		+ "If certain literals should be ignored this can be specified by "
		+ "defining ignore patterns. It may be advisable to generally ignore"
		+ "one digit literals by using pattern '\\d' or explicitly specifiying "
		+ "literals '0' and '1'. Otherwise the analyzes will find a lot of "
		+ "redundant literals.")
public class RedundantLiteralAnalyzer extends
		ConQATPipelineProcessorBase<ITokenResource> {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The default key used for storing the findings for redundant literals", type = "java.util.List<Finding>")
	public static final String DEFAULT_KEY = "Redundant Literals";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.WRITEKEY_NAME, attribute = ConQATParamDoc.WRITEKEY_KEY_NAME, optional = true, description = "The key to store the findings in. Default is "
			+ DEFAULT_KEY)
	public String findingsKey = DEFAULT_KEY;

	/** This maps from a literal string to all tokens that contain the literal. */
	private final ListMap<String, IToken> literals = new ListMap<String, IToken>();

	/** Finding group */
	private FindingGroup group;

	/** Maps from uniform path to element. */
	private Map<String, ITokenElement> uniformPathToElementMap;

	/** Support object that provides means for analyzing literals. */
	@AConQATParameterObject
	public final LiteralAnalyzerSupport literalAnalyzer = new LiteralAnalyzerSupport() {

		/** {@inheritDoc} */
		@Override
		public void processLiteral(IToken token, ITokenElement element) {
			literals.add(token.getText(), token);
		}
	};

	/** {@ConQATDoc} */
	@AConQATFieldParameter(attribute = "value", description = "Literal occurences up to this number are not reported. Default is 1.", parameter = "threshold", optional = true)
	public int threshold = 1;

	/** {@inheritDoc} */
	@Override
	public void processInput(ITokenResource input) throws ConQATException {
		FindingCategory category = NodeUtils.getFindingReport(input)
				.getOrCreateCategory("Redundancy");
		// no natural grouping, hence group is equal to category
		group = category.createFindingGroup("Redundant Literals");

		uniformPathToElementMap = ResourceTraversalUtils
				.createUniformPathToElementMap(input, ITokenElement.class);

		NodeUtils.addToDisplayList(input, findingsKey);
		literalAnalyzer.processLiterals(input, getLogger());

		for (String literal : literals.getKeys()) {
			List<IToken> tokens = literals.getCollection(literal);

			if (tokens.size() > threshold) {
				createFinding(literal, tokens);
			}
		}
	}

	/** Create finding for redundant literal. */
	private void createFinding(String literal, List<IToken> tokens)
			throws ConQATException {
		String message = "Literal '" + literal + "' appears in "
				+ tokens.size() + " locations.";

		for (IToken token : tokens) {
			ITokenElement element = obtainElement(token);
			TextRegionLocation location = ResourceUtils
					.createTextRegionLocationForFilteredOffsets(element,
							token.getOffset(), token.getEndOffset());
			FindingUtils.createAndAttachFinding(group, message, element,
					location, findingsKey);
		}
	}

	/** Get element this token belongs to. */
	private ITokenElement obtainElement(IToken token) {
		ITokenElement element = uniformPathToElementMap
				.get(token.getOriginId());
		CCSMAssert.isNotNull(element, "Must have been stored before.");
		return element;
	}

}