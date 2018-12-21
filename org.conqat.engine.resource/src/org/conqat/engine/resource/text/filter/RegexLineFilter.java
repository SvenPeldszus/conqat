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
package org.conqat.engine.resource.text.filter;

import java.util.List;

import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.text.filter.base.Deletion;
import org.conqat.engine.resource.text.filter.base.LineBasedTextFilterBase;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 41499 $
 * @ConQAT.Rating GREEN Hash: C2D05F8F157EC09821CE5F12E9A501FF
 */
@AConQATProcessor(description = "A filter that removes all lines based on inclusion / exclusion patterns.")
public class RegexLineFilter extends LineBasedTextFilterBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "include-patterns", attribute = "ref", description = ""
			+ "Patterns that specify lines to be kept. Include patterns are overruled by exclude patterns."
			+ "If no include patterns are provided, all lines (except excluded ones) are kept. ")
	public PatternList includePatterns;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "exclude-patterns", attribute = "ref", description = ""
			+ "Patterns that specify lines to be removed. Exclude patterns overrule include patterns.")
	public PatternList excludePatterns;

	/** {@inheritDoc} */
	@Override
	protected void getDeletionsForLine(String fullString, int start, int end,
			List<Deletion> deletions) {
		if (!isIncluded(fullString.substring(start, end))) {
			if (end < fullString.length() && fullString.charAt(end) == '\n') {
				end++;
			}
			deletions.add(new Deletion(start, end, false));
		}
	}

	/**
	 * Whether the given line should be included, i.e. matches at least one
	 * include pattern (if there are any) but no exclude patterns.
	 */
	private boolean isIncluded(String s) {
		return (includePatterns.isEmpty() || includePatterns.findsAnyIn(s))
				&& !excludePatterns.findsAnyIn(s);
	}
}