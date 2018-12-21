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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.BuildConfiguration;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.util.TextElementXMLReader;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.xml.IXMLElementProcessor;

/**
 * Base class for parsers of VS.NET project elements.
 * <p>
 * Since different versions of the Visual Studio generate different project
 * formats, different project parsers exist. This class serves as factory to
 * create a {@link ProjectParser} for a specific VS.NET version.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51331 $
 * @ConQAT.Rating RED Hash: 2AA8CBFA23914FA94CA3978EE56E5A5F
 */
public abstract class ProjectParser
		extends
		TextElementXMLReader<EProjectXmlElement, EProjectXmlAttribute, ConQATException> {

	/** The VisualStudio project that will be returned by this parser. */
	private final VSProject vsProject = new VSProject(getLocation());

	/** Constructor */
	public ProjectParser(ITextElement project) throws ConQATException {
		super(project, EProjectXmlAttribute.class);
	}

	/**
	 * Factory method that creates a {@link ProjectParser} according to the
	 * solution version by parsing the project file.
	 * 
	 * @throws ConQATException
	 *             if the solution format cannot be determined.
	 */
	// TODO (AG) Actually, it does not only create a parser, but also invokes it
	// and returns the parsed result. -> Update comment?
	// TODO (AG) Rename parameter? "Project" sounds too similar to an instance
	// of VSProject.
	public static VSProject parse(ITextElement project) throws ConQATException,
			AssertionError {
		switch (ESolutionFormatVersion.determineProjectFormat(project)) {
		case VERSION_9:
			return new ProjectParser9(project).parse();
		case VERSION_8:
			return new ProjectParser8(project).parse();
		default:
			throw new ConQATException(
					"No reader for this project format implemented!");
		}
	}

	/**
	 * Parses the project file and returns a object to access project
	 * properties.
	 */
	public VSProject parse() throws ConQATException {
		parseAndWrapExceptions();

		processDecendantElements(createSourceFileProcessor());
		processDecendantElements(createPropertyProcessor());

		return vsProject;
	}

	/** Creates a parser for reading the source file names. */
	public abstract SourceFileProcessor createSourceFileProcessor();

	/** Creates a parser for reading the project properties. */
	public abstract PropertyProcessor createPropertyProcessor();

	/**
	 * Parser to be returned by {@link #createPropertyProcessor()} of inheriting
	 * classes.
	 * <p>
	 * This class offers a method to set the source files of the
	 * {@link VSProject} ({@link #addSourceFile(String)}).
	 */
	protected abstract class SourceFileProcessor implements
			IXMLElementProcessor<EProjectXmlElement, ConQATException> {

		/** Adds a source file to the {@link VSProject}. */
		public void addSourceFile(String file) {
			vsProject.relativeSources.add(file);
		}
	}

	/**
	 * Parser to be returned by {@link #createPropertyProcessor()} of inheriting
	 * classes.
	 * <p>
	 * This class offers methods to set the properties of the {@link VSProject}.
	 */
	protected abstract class PropertyProcessor implements
			IXMLElementProcessor<EProjectXmlElement, ConQATException> {

		/** Sets the warning level for the build configuration. */
		protected void setWarningLevel(BuildConfiguration config,
				int warningLevel) {
			vsProject.warningLevel.set(config, warningLevel);
		}

		/** Sets the output path for the build configuration. */
		protected void setOutputPath(BuildConfiguration config,
				String outputPath) {
			vsProject.outputPath.set(config, outputPath);
		}

		/** Sets the output type for the build configuration. */
		protected void setOutputType(BuildConfiguration config,
				String outputType) {
			vsProject.outputType.set(config, outputType);
		}

		/** Sets the assembly name for the build configuration. */
		protected void setAssemblyName(BuildConfiguration config,
				String assemblyName) {
			vsProject.assemblyName.set(config, assemblyName);
		}

		/**
		 * Sets the no warn id string for the build configuration. The warning
		 * IDs are split into integers and stored in a set.
		 */
		protected void setNoWarnString(BuildConfiguration config,
				String noWarnString) {
			HashSet<Integer> noWarnIds = new HashSet<>();
			for (String id : noWarnString.split("[,;]")) {
				id = id.trim();
				if (!id.isEmpty()) {
					noWarnIds.add(Integer.parseInt(id));
				}
			}
			vsProject.noWarnIds.set(config, noWarnIds);
		}
	}

	// TODO (AG) Extract class to separate file? This is used from elsewhere.
	/** Stores data related to a VisualStudio project file. */
	public static class VSProject {

		/** The default warning level used if no level is specified. */
		private static final int DEFAULT_WARNING_LEVEL = 4;

		/** Set of sources filenames relative to the project file. */
		private final Set<String> relativeSources = new HashSet<>();

		/** Warning Level used for this configuration */
		private final ConfigurationProperty<Integer> warningLevel = new ConfigurationProperty<>();

		/** The name of the assembly */
		private final ConfigurationProperty<String> assemblyName = new ConfigurationProperty<>();

		/** The relative path to the assembly */
		private final ConfigurationProperty<String> outputPath = new ConfigurationProperty<>();

		/** The output type (can be either Library or Exe) */
		// TODO (AG) ...or WinExe, see l. 258
		private final ConfigurationProperty<String> outputType = new ConfigurationProperty<>();

		/** List of Ids that suppress compiler warnings by id. */
		private final ConfigurationProperty<Set<Integer>> noWarnIds = new ConfigurationProperty<>();

		/** The location of the project file. */
		private final String location;

		/** Constructor. */
		private VSProject(String location) {
			this.location = location;
		}

		/** Returns the relative sources referenced in this project. */
		public Set<String> getRelativeSources() {
			return CollectionUtils.asUnmodifiable(relativeSources);
		}

		/**
		 * Returns a {@link Set} of suppressed warning ids for the given
		 * configuration
		 */
		public Set<Integer> getNoWarnIds(BuildConfiguration config) {
			Set<Integer> noWarnIds = this.noWarnIds.get(config);
			if (noWarnIds == null) {
				return CollectionUtils.emptySet();
			}
			return CollectionUtils.asUnmodifiable(noWarnIds);
		}

		/**
		 * Extracts the relative name to the assembly that is generated when
		 * compiling this project.
		 * 
		 * @throws ConQATException
		 *             If the assembly name was not configured.
		 */
		public String getRelativeAssemblyName(BuildConfiguration config)
				throws ConQATException {

			String outputType = this.outputType.get(config);
			String assemblyName = this.assemblyName.get(config);

			if (outputType == null || assemblyName == null) {
				// This happens if .vcproj-files are parsed (CR#2556).
				throw new ConQATException(
						"No valid assembly name was identified in the project "
								+ location + ".");
			}

			String outputPath = this.outputPath.get(config);
			if (outputPath == null) {
				throw new ConQATException(
						"No relative output path found for project " + location
								+ ". Perhaps the build configuration ("
								+ config.getId()
								+ ") is not valid for the project.");
			}

			String separator = StringUtils.EMPTY_STRING;
			if (!StringUtils.endsWithOneOf(outputPath, "\\", "/")) {
				separator = "\\";
			}
			return outputPath + separator + assemblyName
					+ getAssemblyExtension(outputType, assemblyName);
		}

		/** @return the assembly extension for the given ouput type. */
		private String getAssemblyExtension(String outputType,
				String assemblyName) throws ConQATException {
			switch (outputType) {
			case "Library":
				return ".dll";
			case "Exe":
			case "WinExe":
				return ".exe";
			default:
				throw new ConQATException("The assembly " + assemblyName
						+ " that was found in project " + location
						+ " is neither a .dll nor a .exe");
			}
		}

		/**
		 * Returns the warning level for the given configuration. May be
		 * {@link #DEFAULT_WARNING_LEVEL} if not defined.
		 */
		public int getWarningLevel(BuildConfiguration config) {
			Integer warningLevel = this.warningLevel.get(config);
			if (warningLevel != null) {
				return warningLevel;
			}
			return DEFAULT_WARNING_LEVEL;
		}

	}

	/**
	 * Stores a project configuration property for different build
	 * configurations with fallback to the global configuration.
	 */
	// TODO (AG) What is the global configuration and where does it come from?
	// Seems that this is implicitly defined by a null config. This should be
	// also mentioned here, not only in the set method's comment.
	private static class ConfigurationProperty<T> {

		/** Key for storing global properties. */
		private static final String GLOBAL_PROPERTIES_KEY = StringUtils.EMPTY_STRING;

		/** Map of build configuration ids to property values. */
		private final HashMap<String, T> values = new HashMap<>();

		/** @return the global configuration value or <code>null</code>. */
		public T get() {
			return values.get(GLOBAL_PROPERTIES_KEY);
		}

		/**
		 * @return the configuration value for the specified build configuration
		 *         or global configuration value or <code>null</code>.
		 */
		public T get(BuildConfiguration config) {
			T value = values.get(getConfigKey(config));
			if (value != null) {
				return value;
			}

			return get();
		}

		/**
		 * Sets the configuration value for a given build configuration. If the
		 * build configuration is null, it is stored as global value.
		 */
		public void set(BuildConfiguration config, T value) {
			values.put(getConfigKey(config), value);
		}

		/** Returns the storage key for a build configuration. */
		private static String getConfigKey(BuildConfiguration config) {
			if (config != null) {
				return config.getId();
			}
			return GLOBAL_PROPERTIES_KEY;
		}
	}
}