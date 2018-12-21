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
package org.conqat.engine.commons.statistics;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.conqat.engine.commons.ConQATPipelineProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: pfaller $
 * @version $Rev: 48808 $
 * @ConQAT.Rating GREEN Hash: 66A94FDD6340F4B7A335859199830E90
 */
@AConQATProcessor(description = "Removes items from a KeyedData structure to meet the given length threshold. "
		+ "The items from the beginning of the structure (determined by the iteration order) are kept.")
public class KeyedDataTopFilter extends
		ConQATPipelineProcessorBase<KeyedData<Comparable<?>>> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "top", attribute = "value", optional = false, description = ""
			+ "The maximaum number of items to pass on.")
	public int numberOfItems;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "summarize", attribute = "others", optional = true, description = ""
			+ "Whether there should be an item for \"Others\".")
	public boolean summarizeOthers = true;

	/** {@inheritDoc} */
	@Override
	protected void processInput(KeyedData<Comparable<?>> keyedData) {
		Map<Comparable<?>, Double> values = keyedData.getValues();
		if (values.size() <= numberOfItems) {
			return;
		}

		double othersSum = removeAndSumUpAfter(values, numberOfItems);
		if (summarizeOthers) {
			values.put("Others", othersSum);
		}
	}

	/**
	 * Keeps the first n elements of the original map, deletes the remaining
	 * elements and returns the sum of all deleted elements' values.
	 */
	public static <K> double removeAndSumUpAfter(Map<K, Double> input, int n) {

		int length = Math.min(n, input.size());
		Iterator<Entry<K, Double>> it = input.entrySet().iterator();
		for (int i = 0; i < length; i++) {
			it.next();
		}

		double sum = 0;
		while (it.hasNext()) {
			sum += it.next().getValue();
			it.remove();
		}
		return sum;
	}

}
