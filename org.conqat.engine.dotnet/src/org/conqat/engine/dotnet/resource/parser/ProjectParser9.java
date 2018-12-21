/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2012 The ConQAT Project                                   |
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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.BuildConfiguration;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.xml.IXMLElementProcessor;

/**
 * Parses VS.NET 2005 and newer projects.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51331 $
 * @ConQAT.Rating RED Hash: 96EBDFEB4F3658DC1C86863D44B7F3A5
 */
/* package */class ProjectParser9 extends ProjectParser {

	/** Regex pattern for parsing the build configuration. */
	// TODO (AG) Mention what the capturing groups correspond to (config name
	// and platform), and provide an example string.
	private static final Pattern CONDITION_PATTERN = Pattern
			.compile(".*==\\s*'?(.*?)\\|([^']*).*");

	/** Constructor */
	protected ProjectParser9(ITextElement project) throws ConQATException {
		super(project);
	}

	/** {@inheritDoc} */
	@Override
	public SourceFileProcessor createSourceFileProcessor() {
		return new ItemGroupProcessor();
	}

	/** {@inheritDoc} */
	@Override
	public PropertyProcessor createPropertyProcessor() {
		return new PropertyGroupProcessor();
	}

	/** Processor for ItemGroup elements */
	private class ItemGroupProcessor extends SourceFileProcessor {

		/** {@inheritDoc} */
		@Override
		public EProjectXmlElement getTargetElement() {
			return EProjectXmlElement.ItemGroup;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			processChildElements(new CompileProcessor());
		}

		/** Processor for Compile elements */
		private class CompileProcessor implements
				IXMLElementProcessor<EProjectXmlElement, ConQATException> {

			/** {@inheritDoc} */
			@Override
			public EProjectXmlElement getTargetElement() {
				return EProjectXmlElement.Compile;
			}

			/** {@inheritDoc} */
			@Override
			public void process() {
				addSourceFile(getStringAttribute(EProjectXmlAttribute.Include));
			}
		}
	}

	/** Processor for {@link EProjectXmlElement#PropertyGroup} elements */
	private class PropertyGroupProcessor extends PropertyProcessor {
		/** {@inheritDoc} */
		@Override
		public EProjectXmlElement getTargetElement() {
			return EProjectXmlElement.PropertyGroup;
		}

		/** {@inheritDoc} */
		@Override
		public void process() {
			BuildConfiguration config = null;
			String condition = getStringAttribute(EProjectXmlAttribute.Condition);

			if (!StringUtils.isEmpty(condition)) {
				Matcher matcher = CONDITION_PATTERN.matcher(condition);
				if (!matcher.matches()) {
					return; // no valid build configuration condition
				}

				config = new BuildConfiguration(matcher.group(1),
						matcher.group(2));
			}

			parseProperties(config);
		}

		/** Parsing the project properties for the given configuration. */
		private void parseProperties(BuildConfiguration config) {
			String warningLevel = getChildText(EProjectXmlElement.WarningLevel);
			if (warningLevel != null) {
				setWarningLevel(config, Integer.parseInt(warningLevel));
			}

			String assemblyName = getChildText(EProjectXmlElement.AssemblyName);
			if (assemblyName != null) {
				setAssemblyName(config, assemblyName);
			}

			String outputPath = getChildText(EProjectXmlElement.OutputPath);
			if (outputPath != null) {
				setOutputPath(config, outputPath);
			}

			String outputType = getChildText(EProjectXmlElement.OutputType);
			if (outputType != null) {
				setOutputType(config, outputType);
			}

			String noWarnIds = getChildText(EProjectXmlElement.NoWarn);
			if (noWarnIds != null) {
				setNoWarnString(config, noWarnIds);
			}
		}
	}
}