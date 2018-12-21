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

import java.util.Collections;
import java.util.List;

import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.text.filter.base.Deletion;
import org.conqat.engine.resource.text.filter.base.TextFilterBase;
import org.conqat.lib.commons.collections.CollectionUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47521 $
 * @ConQAT.Rating GREEN Hash: 155F7714F0EEF96C4D5448037E41CC38
 */
@AConQATProcessor(description = "A filter that removes the complete content of an element "
		+ "(i.e. treating it as an empty element) based on a regular expression matched "
		+ "against the element's content.")
public class RegexContentFilter extends TextFilterBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "patterns", attribute = "ref", description = "Patterns to check for.")
	public PatternList removePatterns;

	/** {@inheritDoc} */
	@Override
	public List<Deletion> getDeletions(String s, String originUniformPath) {
		if (removePatterns.findsAnyIn(s)) {
			return Collections
					.singletonList(new Deletion(0, s.length(), false));
		}
		return CollectionUtils.emptyList();
	}

}