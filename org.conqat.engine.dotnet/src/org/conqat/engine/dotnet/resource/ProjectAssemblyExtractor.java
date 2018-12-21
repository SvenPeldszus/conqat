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

import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.parser.ProjectParser.VSProject;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: goeb $
 * @version $Rev: 51331 $
 * @ConQAT.Rating RED Hash: 21A5A99DEA93977DE42EA87DAEFCEF37
 */
@AConQATProcessor(description = "This processor provides content accessors for the assemblies "
		+ "belonging to a set of Visual Studio project elements that are input as a text element tree.")
public class ProjectAssemblyExtractor extends ProjectContentExtractorBase {

	/** The build configuration that is used to compile the projects */
	private BuildConfiguration configuration = new BuildConfiguration(
			ConQATParamDoc.DEFAULT_CONFIGURATION_NAME,
			ConQATParamDoc.DEFAULT_PLATFORM);

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "build-configuration", description = "The configuration used to build the solution", minOccurrences = 0, maxOccurrences = 1)
	public void setConfiguration(
			// TODO (AG) Use parameter object for this method and the
			// configuration field. Also in the VSCompilerSettingsAnalyzer.
			// TODO (AG) Mention default values in descriptions.
			@AConQATAttribute(name = "name", defaultValue = ConQATParamDoc.DEFAULT_CONFIGURATION_NAME, description = ConQATParamDoc.DEFAULT_CONFIGURATION_NAME_DESC) String configurationName,
			@AConQATAttribute(name = "platform", defaultValue = ConQATParamDoc.DEFAULT_PLATFORM, description = ConQATParamDoc.DEFAULT_PLATFORM_DESC) String platformName) {
		configuration = new BuildConfiguration(configurationName, platformName);
	}

	/** {@inheritDoc} */
	@Override
	protected Set<String> extractRelativePaths(VSProject project) {
		Set<String> paths = new HashSet<>();

		try {
			paths.add(project.getRelativeAssemblyName(configuration));
		} catch (ConQATException e) {
			getLogger()
					.error("Error extracting assembly: " + e.getMessage(), e);
		}

		return paths;
	}
}