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
package org.conqat.engine.io;

import java.io.File;
import java.io.IOException;

import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * Base class for file writers writing multiple files to a directory.
 * 
 * @author Andreas Goeb
 * @author $Author: kinnen $
 * @version $Rev: 41751 $
 * @ConQAT.Rating GREEN Hash: CF8DB14A23286C7A071FA59B00BF2983
 */
public abstract class MultiFileWriterBase extends ConQATProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "directory", attribute = "name", optional = false, description = "Name of the output directory.")
	public String directory;

	/** {@inheritDoc} */
	@Override
	public final File process() throws ConQATException {

		File outputDir = new File(directory);

		try {
			FileSystemUtils.ensureDirectoryExists(outputDir);
		} catch (IOException e) {
			throw new ConQATException("Could not create directory " + outputDir
					+ ".", e);
		}

		try {
			writeFiles(outputDir);
		} catch (IOException e) {
			throw new ConQATException("Could not write a file to " + outputDir
					+ ".", e);
		}

		return outputDir;
	}

	/** Write files to the output directory. */
	protected abstract void writeFiles(File outputDir) throws ConQATException,
			IOException;

}