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
package org.conqat.engine.io;

import org.conqat.engine.commons.format.IValueFormatter;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;

/**
 * CSV writer using formatters from the display list.
 * 
 * Also the keys to be written as columns are determined by the display list as
 * implemented in {@link CSVWriter#getKeys(IConQATNode)}
 * 
 * @author $Author: streitel $
 * @version $Rev: 51618 $
 * @ConQAT.Rating GREEN Hash: FFB3888FA181CCC746C77C29CB8D609A
 */
@AConQATProcessor(description = "Writes the nodes of a ConQAT node hierarchy to a CSV file. "
		+ "Each node is written to a single line and each line contains the ID of the node and the node's values, "
		+ "formatted as defined in the display list. The first line contains a field description. "
		+ "The keys to be written to the CSV are determined by the display list.")
public class FormattedCSVWriter extends CSVWriter {

	/**
	 * Retrieves the value from a node, using the formatter registered in the
	 * display list of the input's root node. In case of problems, the value is
	 * returned as-is, and an error is logged.
	 */
	@Override
	protected Object getValue(IConQATNode node, String key) {
		Object value = node.getValue(key);

		IValueFormatter formatter = NodeUtils.getDisplayList(
				NodeUtils.getRootNode(node)).getFormatter(key);
		if (formatter == null) {
			return value;
		}

		try {
			return formatter.format(value);
		} catch (ConQATException e) {
			getLogger()
					.error(String
							.format("Problem formatting value %s at node %s with key %s.",
									value, node, key));
			return value;
		}
	}
}
