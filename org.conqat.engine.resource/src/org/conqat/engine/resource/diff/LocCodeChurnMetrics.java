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

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;

/**
 * Calculates code churn metrics in LOC.
 * 
 * @author $Author: pfaller $
 * @version $Rev: 50735 $
 * @ConQAT.Rating YELLOW Hash: 94A52095508B9052B37D2E66CBFEA225
 */
public class LocCodeChurnMetrics extends CodeChurnMetrics<ITextElement> {

	/** Constructor */
	public LocCodeChurnMetrics(ITextElement mainElement,
			ITextElement compareeElement, int maxDeltaSize, IConQATLogger logger)
			throws ConQATException {
		calculateMetrics(mainElement, compareeElement, maxDeltaSize, logger);
	}

	/** {@inheritDoc} */
	@Override
	protected List<String> normalize(ITextElement element, IConQATLogger logger)
			throws ConQATException {
		return TextElementUtils.getNormalizedContent(element);
	}
}
