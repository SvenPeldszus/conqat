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
package org.conqat.engine.java.coverage;

import java.io.StringReader;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.java.library.JavaLibrary;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.scanner.ELanguage;

/**
 * Base class for processors analyzing XML coverage reports for Java.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48762 $
 * @ConQAT.Rating GREEN Hash: 54BE75274A174BF1C081E3E93E92353C
 */
public abstract class JavaCoverageAnalyzerBase extends LineCoverageAnalyzerBase {

	/** Constructor. */
	public JavaCoverageAnalyzerBase() {
		super(ELanguage.JAVA);
	}

	/** {@inheritDoc} */
	@Override
	protected String getQualifiedSourceFileName(ITokenElement element)
			throws ConQATException {
		String className = JavaLibrary.getFQClassName(element.getUniformPath(),
				new StringReader(element.getTextContent()));
		return className.replace('.', '/') + ".java";
	}
}
