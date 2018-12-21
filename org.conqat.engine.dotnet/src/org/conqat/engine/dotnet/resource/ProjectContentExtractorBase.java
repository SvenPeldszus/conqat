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
package org.conqat.engine.dotnet.resource;

import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.parser.ProjectParser;
import org.conqat.engine.dotnet.resource.parser.ProjectParser.VSProject;
import org.conqat.engine.resource.text.ITextElement;

/**
 * Base class for processors that extract content accessors from VS projects.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51330 $
 * @ConQAT.Rating GREEN Hash: 71E4F6776BE3053F9DBDEC7E9C59755D
 */
public abstract class ProjectContentExtractorBase extends
		ContentAccessorExtractorBase {

	/** {@inheritDoc} */
	@Override
	protected Set<String> extractRelativePaths(ITextElement projectElement)
			throws ConQATException {
		VSProject project = ProjectParser.parse(projectElement);
		return extractRelativePaths(project);
	}

	/**
	 * Template method to extract the relevant elements (source or assemblies)
	 * from a project. The paths are interpreted relative to the project
	 * element.
	 */
	protected abstract Set<String> extractRelativePaths(VSProject project);
}