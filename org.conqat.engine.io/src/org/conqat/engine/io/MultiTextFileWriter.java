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
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author Andreas Goeb
 * @author $Author: poehlmann $
 * @version $Rev: 42241 $
 * @ConQAT.Rating GREEN Hash: 36983961EAFD423EBAF1299FCCE3D4A0
 */
@AConQATProcessor(description = "Writes the content of all text elements into files. Can e.g. be used to extract and filter text files from a zip archive. "
		+ "All files are stored directly in the output directory, flattening all hierarchies that exist in the input. If duplicate names exist in the input, "
		+ "error messages are logged for discarded elements.")
public class MultiTextFileWriter extends MultiFileWriterBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "write", attribute = "resource", optional = false, description = "Reference to resource containing elements that get written into files")
	public ITextResource source;

	/** {@inheritDoc} */
	@Override
	protected void writeFiles(File outputDir) throws ConQATException,
			IOException {
		Set<String> seenNames = new HashSet<>();
		for (ITextElement element : ResourceTraversalUtils
				.listTextElements(source)) {
			if (!seenNames.add(element.getName())) {
				getLogger().error(
						"Duplicate file name - output discarded for "
								+ element.getUniformPath());
				continue;
			}
			File outputFile = new File(outputDir, element.getName());
			FileSystemUtils.writeFileUTF8(outputFile, element.getTextContent());
		}
	}
}
