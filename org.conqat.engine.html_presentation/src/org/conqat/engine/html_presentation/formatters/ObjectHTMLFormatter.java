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
package org.conqat.engine.html_presentation.formatters;

import org.conqat.lib.commons.html.HTMLWriter;
import org.conqat.lib.commons.string.StringUtils;

/**
 * A HTML formatter for arbitrary objects using <code>toString()</code>.
 * 
 * @author hummelb
 * @author $Author: kinnen $
 * @version $Rev: 41751 $
 * @ConQAT.Rating GREEN Hash: 73FF9909EE279F55E3D72A2BCE9AF1C5
 */
public class ObjectHTMLFormatter implements IHTMLFormatter<Object> {

	/**
	 * Adds the result of <code>toString()</code> as text, but use the
	 * non-breakable space if the string is empty.
	 */
	@Override
	public void formatObject(Object o, HTMLWriter writer) {
		String string = o.toString();
		if (StringUtils.isEmpty(string)) {
			writer.addRawString("&nbsp;");
		} else {
			writer.addText(string);
		}
	}

}