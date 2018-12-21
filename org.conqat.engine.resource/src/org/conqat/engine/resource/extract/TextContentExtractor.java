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
package org.conqat.engine.resource.extract;

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.text.TextElementProcessorBase;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49687 $
 * @ConQAT.Rating GREEN Hash: 31093329C7D15872CA694CA956F9F1F5
 */
@AConQATProcessor(description = "This processor extracts the content of each element "
		+ "and stores it in a key.")
public class TextContentExtractor extends TextElementProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The content of the element.", type = "java.lang.String")
	public static final String CONTENT_KEY = "content";

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITextResource root) {
		NodeUtils.addToDisplayList(root, CONTENT_KEY);
	}

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITextElement element) throws ConQATException {
		element.setValue(CONTENT_KEY, element.getTextContent());
	}

}