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
package org.conqat.engine.text.comments.analysis;

import org.conqat.engine.commons.filter.FilterBase;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48878 $
 * @ConQAT.Rating GREEN Hash: EED2A8EDCFE44920BB4B910F7B7E48BF
 */
@AConQATProcessor(description = "Reduces a scope to those token resources that have a language supported by the comment analysis.")
public class CommentClassificationSupportedLanguagesFilter extends
		FilterBase<ITokenResource> {

	/** {@inheritDoc} */
	@Override
	protected boolean isFiltered(ITokenResource node) {
		if (!(node instanceof ITokenElement)) {
			// keep inner nodes
			return false;
		}
		ELanguage language = ((ITokenElement) node).getLanguage();
		return !CommentClassificationAnalysisBase.isSupportedLanguage(language);
	}
}
