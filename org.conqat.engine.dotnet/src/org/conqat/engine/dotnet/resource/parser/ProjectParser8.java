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
package org.conqat.engine.dotnet.resource.parser;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.BuildConfiguration;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.xml.IXMLElementProcessor;

/**
 * Parses VS.NET 2003 projects.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51330 $
 * @ConQAT.Rating GREEN Hash: 20D27AC48414516152D3A232F62FDD37
 */
/* package */class ProjectParser8 extends ProjectParser {

	/** Constructor */
	protected ProjectParser8(ITextElement project) throws ConQATException {
		super(project);
	}

	/** {@inheritDoc} */
	@Override
	public SourceFileProcessor createSourceFileProcessor() {
		return new IncludeProcessor();
	}

	/** {@inheritDoc} */
	@Override
	public PropertyProcessor createPropertyProcessor() {
		return new SettingsProcessor();
	}

	/** Processor for {@link EProjectXmlElement#Include} elements */
	private class IncludeProcessor extends SourceFileProcessor {

		/** {@inheritDoc} */
		@Override
		public EProjectXmlElement getTargetElement() {
			return EProjectXmlElement.Include;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			processChildElements(new FileProcessor());
		}

		/** Processor for File elements */
		private class FileProcessor implements
				IXMLElementProcessor<EProjectXmlElement, ConQATException> {

			/** {@inheritDoc} */
			@Override
			public EProjectXmlElement getTargetElement() {
				return EProjectXmlElement.File;
			}

			/** {@inheritDoc} */
			@Override
			public void process() {
				String relativeSourceElementName = getStringAttribute(EProjectXmlAttribute.RelPath);
				String buildAction = getStringAttribute(EProjectXmlAttribute.BuildAction);
				if (buildAction != null && buildAction.equals("Compile")) {
					String link = getStringAttribute(EProjectXmlAttribute.Link);
					if (!StringUtils.isEmpty(link)) {
						relativeSourceElementName = link;
					}

					addSourceFile(relativeSourceElementName);
				}
			}
		}
	}

	/** Processor for {@link EProjectXmlElement#Settings} elements */
	private class SettingsProcessor extends PropertyProcessor {
		/** {@inheritDoc} */
		@Override
		public EProjectXmlElement getTargetElement() {
			return EProjectXmlElement.Settings;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			parseProperties(null);
			processChildElements(new ConfigProcessor());
		}

		/** Parses the project properties for the given build configuration. */
		private void parseProperties(BuildConfiguration config) {
			if (hasAttribute(EProjectXmlAttribute.WarningLevel)) {
				setWarningLevel(config,
						getIntAttribute(EProjectXmlAttribute.WarningLevel));
			}
			if (hasAttribute(EProjectXmlAttribute.AssemblyName)) {
				setAssemblyName(config,
						getStringAttribute(EProjectXmlAttribute.AssemblyName));
			}
			if (hasAttribute(EProjectXmlAttribute.OutputPath)) {
				setOutputPath(config,
						getStringAttribute(EProjectXmlAttribute.OutputPath));
			}
			if (hasAttribute(EProjectXmlAttribute.OutputType)) {
				setOutputType(config,
						getStringAttribute(EProjectXmlAttribute.OutputType));
			}
			if (hasAttribute(EProjectXmlAttribute.NoWarn)) {
				setNoWarnString(config,
						getStringAttribute(EProjectXmlAttribute.NoWarn));
			}
		}

		/** Processor for {@link EProjectXmlElement#Config} elements */
		private class ConfigProcessor implements
				IXMLElementProcessor<EProjectXmlElement, ConQATException> {

			/** {@inheritDoc} */
			@Override
			public EProjectXmlElement getTargetElement() {
				return EProjectXmlElement.Config;
			}

			/** {@inheritDoc} */
			@Override
			public void process() {
				BuildConfiguration config = new BuildConfiguration(
						getStringAttribute(EProjectXmlAttribute.Name), null);
				parseProperties(config);
			}
		}
	}

}