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
package org.conqat.engine.html_presentation.image;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 49316 $
 * @ConQAT.Rating YELLOW Hash: FD1C7388A264EEEA571DE67F4DAD417D
 */
@AConQATProcessor(description = "This processor renders an image descriptor to an HTML page.")
public class HTMLImageRenderer extends HTMLImageRendererBase {

	/** {@ConQAT.Doc} */
	@Override
	@AConQATParameter(name = "image", minOccurrences = 1, description = "Image descriptors to render. "
			+ "If more than one image descriptor is provided, all information (such as tooltips) are based on the first image and links to switch between the images are created.")
	public void addImageDescriptor(
			@AConQATAttribute(name = ConQATParamDoc.INPUT_REF_NAME, description = "The image descriptor.") IImageDescriptor descriptor,
			@AConQATAttribute(name = "name", defaultValue = "Image", description = "The name used for the links that switch between the images.") String name) {
		super.addImageDescriptor(descriptor, name);
	}
}