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
package org.conqat.engine.text.msword;

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.text.TextFilterAwareElementFactoryBase;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 48970 $
 * @ConQAT.Rating GREEN Hash: 06192E4B347FF6369061A2AE19CED0B2
 */
@AConQATProcessor(description = "Factory for treating MS Word files as text files.")
public class MSWordTextElementFactory extends TextFilterAwareElementFactoryBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "wrap-at-dot", attribute = "value", optional = true, description = ""
			+ "If set to true, a newline is inserted after each dot (default is false).")
	public boolean wrapAtDot = false;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "wrap-at-whitespace", attribute = "value", optional = true, description = ""
			+ "If set to true, a newline is inserted after each whitespace block (default is false).")
	public boolean wrapAtWhitespace = false;

	/** {@inheritDoc} */
	@Override
	public IElement create(IContentAccessor accessor) {
		return new MSWordTextElement(accessor, getFilters(), wrapAtDot,
				wrapAtWhitespace);
	}
}