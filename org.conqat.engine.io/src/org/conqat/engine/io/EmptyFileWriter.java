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

import org.conqat.engine.core.core.AConQATProcessor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: goeb $
 * @version $Rev: 50992 $
 * @ConQAT.Rating YELLOW Hash: AFDE8A8EDC159D76D06E1435C717B0FA
 */
@AConQATProcessor(description = "Creates an empty file, if a file with the given name does not already exist. This can be used to create marker files.")
public class EmptyFileWriter extends FileWriterBase {

	/** {@inheritDoc} */
	@Override
	protected void writeFile(File file) throws IOException {
		file.createNewFile();
	}
}
