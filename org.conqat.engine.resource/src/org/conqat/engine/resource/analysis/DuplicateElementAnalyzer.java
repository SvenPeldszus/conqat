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
package org.conqat.engine.resource.analysis;

import java.util.List;

import org.conqat.engine.commons.node.ListNode;
import org.conqat.engine.commons.node.NodeConstants;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.util.ConQATInputProcessorBase;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49411 $
 * @ConQAT.Rating GREEN Hash: D9FCF655579F98123F21A39A5C2FFF29
 */
@AConQATProcessor(description = "This processor identifies identical elements."
		+ " As output it generates a list of the identical elements with the"
		+ " number of instances and the locations of the duplicates.")
public class DuplicateElementAnalyzer extends
		ConQATInputProcessorBase<IResource> {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The number of copies of the element.", type = "java.lang.Integer")
	public static final String COPIES_COUNT_KEY = "#Copies";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The locations of the copies.", type = "java.util.List<String>")
	public static final String LOCATIONS_KEY = "Locations";

	/** Processor for detecting duplicate elements. */
	@AConQATParameterObject
	public final DuplicateElementProcessorSupport processor = new DuplicateElementProcessorSupport() {

		/** Adds all duplicates to the result node. */
		@Override
		protected void processDuplicate(List<IElement> elements) {
			ListNode node = new ListNode(elements.get(0).getName());
			result.addChild(node);

			node.setValue(COPIES_COUNT_KEY, elements.size());
			List<String> locations = NodeUtils.getOrCreateStringList(node,
					LOCATIONS_KEY);

			for (IElement element : elements) {
				locations.add(element.getLocation());
			}
		}
	};

	/** The result node. */
	private ListNode result;

	/** {@inheritDoc} */
	@Override
	public ListNode process() throws ConQATException {
		result = new ListNode();
		result.setValue(NodeConstants.HIDE_ROOT, true);
		NodeUtils.addToDisplayList(result, COPIES_COUNT_KEY, LOCATIONS_KEY);

		processor.processDuplicates(input);

		return result;
	}

}