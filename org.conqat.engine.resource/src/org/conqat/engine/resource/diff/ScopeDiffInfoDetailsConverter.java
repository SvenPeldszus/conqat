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
package org.conqat.engine.resource.diff;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.node.ListNode;
import org.conqat.engine.commons.node.NodeConstants;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.util.ConQATInputProcessorBase;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.Pair;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: pfaller $
 * @version $Rev: 50003 $
 * @ConQAT.Rating YELLOW Hash: 720C00EEBD26005028040D54C830BE94
 */
@AConQATProcessor(description = "This process creates a node structure that for each element shows the modification "
		+ "type (added, removed, modified, unmodified) and various churn metrics. Additionally, this processor can"
		+ "calculate the delta for specified numerical values.")
public class ScopeDiffInfoDetailsConverter extends
		ConQATInputProcessorBase<ScopeDiffInfo> {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "", type = "java.lang.String")
	public static final String KEY_TYPE = "Type";

	/** Keys to copy. */
	private final static List<String> KEYS = Arrays.asList(
			ScopeDiffer.KEY_NORMALIZED_LINES, ScopeDiffer.KEY_CHURN_LINES,
			ScopeDiffer.KEY_RELATIVE_CHURN);

	/** Key to calculate delta for. */
	private final Set<String> deltaKeys = new LinkedHashSet<>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = ConQATParamDoc.READKEY_NAME, description = "Key to create delta for. If the key is not present in one of "
			+ "the scope or not a number, we simply assume the value zero.")
	public void addKey(
			@AConQATAttribute(name = ConQATParamDoc.READKEY_KEY_NAME, description = ConQATParamDoc.READKEY_KEY_DESC) String key) {
		deltaKeys.add(key);
	}

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.LOG_LEVEL_NAME, attribute = ConQATParamDoc.ATTRIBUTE_VALUE_NAME, description = ConQATParamDoc.LOG_LEVEL_DESCRIPTION
			+ ConQATParamDoc.LOG_LEVEL_DESCRIPTION + " [default: WARN]", optional = true)
	public ELogLevel logLevel = ELogLevel.WARN;

	/** {@inheritDoc} */
	@Override
	public ListNode process() throws ConQATException {

		if (!input.hasValidLineChurn()) {
			getLogger().log(
					logLevel,
					"Line churn information appears to be incomplete. Hence, results "
							+ "of this processor may be incomplete, too.");
		}

		ListNode root = new ListNode();
		root.setValue(NodeConstants.HIDE_ROOT, true);

		NodeUtils.addToDisplayList(root, KEY_TYPE);

		for (String deltaKey : deltaKeys) {
			NodeUtils.addToDisplayList(root, obtainDeltaKey(deltaKey));
		}

		NodeUtils.addToDisplayList(root, KEYS);

		addElements(root, "New", input.getAddedElements());
		addElements(root, "Removed", input.getRemovedElements());
		addElements(root, "Modified", input.getModifiedElements());
		addElements(root, "Umodified", input.getUnmodifiedElements());
		return root;
	}

	/**
	 * Obtain name of delta key for a givne key.
	 */
	private String obtainDeltaKey(String key) {
		return "Delta " + key;
	}

	/**
	 * Add all elements.
	 */
	private void addElements(ListNode parent, String type,
			Set<ITextElement> elements) throws ConQATException {
		for (ITextElement element : elements) {
			ListNode child = new ListNode(element.getUniformPath());
			child.setValue(KEY_TYPE, type);

			copyValues(element, child);

			createDeltaInfo(element, child);

			parent.addChild(child);
		}

	}

	/** Copy the values. */
	private void copyValues(ITextElement element, ListNode child) {
		try {
			NodeUtils.copyValues(KEYS, element, child, false);
		} catch (ConQATException e) {
			CCSMAssert.fail("Should not be thrown as we do not deep clone");
		}
	}

	/** Create the delta info for the delta keys. */
	private void createDeltaInfo(ITextElement element, ListNode child)
			throws ConQATException {
		for (String deltaKey : deltaKeys) {
			Pair<Number, Number> values = input.getValues(
					element.getUniformPath(), deltaKey, Number.class, 0d);
			child.setValue(obtainDeltaKey(deltaKey), values.getFirst()
					.doubleValue() - values.getSecond().doubleValue());
		}
	}

}
