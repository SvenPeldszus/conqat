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
package org.conqat.engine.io;

import java.io.File;
import java.io.IOException;

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 42241 $
 * @ConQAT.Rating GREEN Hash: 9F583F87F3314E14C603F191ECCC2F7F
 */
@AConQATProcessor(description = "Writes the content from a single text element into a file. Can e.g. be used to extract a file from a zip archive."
		+ "Other than SingleFileWriter, this processor removes filtered parts from the text content.")
public class SingleTextFileWriter extends FileWriterBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "write", attribute = "element", optional = false, description = "Reference to element that gets written into a file")
	public ITextResource source;

	/** {@inheritDoc} */
	@Override
	protected void writeFile(File file) throws ConQATException, IOException {
		FileSystemUtils.writeFileUTF8(file, ResourceTraversalUtils
				.getSingleTextElement(source).getTextContent());
	}

}
