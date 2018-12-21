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
package org.conqat.engine.resource.filters;

import java.util.Collections;
import java.util.List;

import org.conqat.engine.commons.ConQATPipelineProcessorBase;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.analysis.DuplicateElementProcessorSupport;
import org.conqat.engine.resource.util.ElementUniformPathComparator;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49411 $
 * @ConQAT.Rating GREEN Hash: E918DC3FCEAD9A4B10ECB41723CCC9CA
 */
@AConQATProcessor(description = "This processor identifies identical elements "
		+ "and filters them so that only one instance remains in the scope. "
		+ "This is very useful when analyzing directories that contain "
		+ "multiple copies of a system, e.g. in the form of dist directories.")
public class DuplicateElementFilter extends
		ConQATPipelineProcessorBase<IResource> {

	/** Counts filtered elements. */
	private int count = 0;

	/** Processor for detecting duplicate elements. */
	@AConQATParameterObject
	public final DuplicateElementProcessorSupport processor = new DuplicateElementProcessorSupport() {
		/**
		 * For every list of duplicates this filters all elements but the first
		 * one in the order according the uniform path.
		 */
		@Override
		protected void processDuplicate(List<IElement> elements) {

			// sort list to ensure deterministic behavior
			Collections.sort(elements, new ElementUniformPathComparator());

			for (int i = 1; i < elements.size(); i++) {
				elements.get(i).remove();
				count++;
			}
		}
	};

	/** {@inheritDoc} */
	@Override
	public void processInput(IResource input) throws ConQATException {
		processor.processDuplicates(input);
		getLogger().info("Filtered " + count + " duplicate elements.");
	}

}