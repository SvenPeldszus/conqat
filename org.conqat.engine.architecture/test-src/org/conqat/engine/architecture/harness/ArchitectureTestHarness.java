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
package org.conqat.engine.architecture.harness;

import java.io.File;

import org.conqat.engine.architecture.scope.ArchitectureDefinition;
import org.conqat.engine.architecture.scope.ArchitectureDefinitionReader;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.ProcessorInfoMock;

/**
 * Test harness for the architecture bundle.
 * 
 * @author heineman
 * @author $Author: juergens $
 * @version $Rev: 35037 $
 * @ConQAT.Rating GREEN Hash: CC982E01541FB186ABD157BEAABADB43
 */
public class ArchitectureTestHarness {

	/**
	 * Reads the architecture from the given file.
	 */
	public static ArchitectureDefinition readArchitecture(File file)
			throws ConQATException {
		ArchitectureDefinitionReader reader = new ArchitectureDefinitionReader();
		reader.init(new ProcessorInfoMock());
		reader.setInputFile(file);
		return reader.process();
	}

}