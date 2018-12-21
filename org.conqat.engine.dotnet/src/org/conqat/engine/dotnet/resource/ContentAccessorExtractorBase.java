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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.logging.IncludeExcludeListLogMessage;
import org.conqat.engine.commons.logging.StructuredLogTags;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.dotnet.util.MappingFileUtils;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.scope.PatternSupportingScopeBase;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.SetMap;

/**
 * Base class for processors that extract {@link IContentAccessor}s from scope
 * descriptors (given as {@link ITextResource}s). A scope descriptor is a file,
 * which contains a list of file paths that define the actual scope. An example
 * are the source files defined in a Visual Studio Project file.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51320 $
 * @ConQAT.Rating GREEN Hash: 3C8507E785A828EC87494A1DDE92D0BE
 */
public abstract class ContentAccessorExtractorBase extends
		PatternSupportingScopeBase {

	/** Root node of input */
	protected ITextResource input;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "mapping-file", attribute = "path", optional = true, description = "If set, mapping from scope descriptor elements to extracted content accessors is written to this file.")
	public String mappingFile;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = ConQATParamDoc.INPUT_NAME, description = ConQATParamDoc.INPUT_DESC, minOccurrences = 1, maxOccurrences = 1)
	public void setInput(
			@AConQATAttribute(name = ConQATParamDoc.INPUT_REF_NAME, description = ConQATParamDoc.INPUT_REF_DESC) ITextResource input) {
		this.input = input;
	}

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.LOG_LEVEL_NAME, attribute = ConQATParamDoc.ATTRIBUTE_VALUE_NAME, description = ConQATParamDoc.LOG_LEVEL_DESCRIPTION
			+ ConQATParamDoc.LOG_LEVEL_DESCRIPTION + " [default: ERROR]", optional = true)
	public ELogLevel logLevel = ELogLevel.ERROR;

	/** {@inheritDoc} */
	@Override
	public IContentAccessor[] createAccessors() throws ConQATException {

		// Maps from uniform path of scope descriptor to content accessors.
		SetMap<String, IContentAccessor> scopeDescriptorsToAccessors = new SetMap<String, IContentAccessor>();

		Set<String> excludedPaths = new HashSet<String>();

		for (ITextElement scopeDescriptor : ResourceTraversalUtils
				.listTextElements(input)) {
			for (String relativePath : extractRelativePaths(scopeDescriptor)) {
				IContentAccessor accessor = getContentAccessor(scopeDescriptor,
						relativePath, excludedPaths);
				if (accessor != null) {
					scopeDescriptorsToAccessors.add(
							scopeDescriptor.getUniformPath(), accessor);
				}
			}
		}

		Set<IContentAccessor> includedAccessors = scopeDescriptorsToAccessors
				.getValues();

		logIncludedElements(includedAccessors);
		if (excludedPaths.size() > 0) {
			logExcludedElements(excludedPaths);
		}

		if (mappingFile != null) {
			MappingFileUtils.writeMapping(mappingFile,
					scopeDescriptorsToAccessors);
		}

		return CollectionUtils.toArray(includedAccessors,
				IContentAccessor.class);
	}

	/**
	 * Template method that extracts a set of paths from the scope descriptor
	 * text element. The paths are interpreted relative to the scope
	 * descriptor's location.
	 */
	protected abstract Set<String> extractRelativePaths(
			ITextElement scopeDescriptor) throws ConQATException;

	/**
	 * Returns the content accessor corresponding to the path relative to the
	 * scope descriptor. If the path is excluded it is added to the set of
	 * excluded paths and <code>null</code> is returned.
	 */
	private IContentAccessor getContentAccessor(ITextElement scopeDescriptor,
			String relativePath, Set<String> excludedPaths) {
		if (compiledExcludePatterns.matchesAny(UniformPathUtils
				.normalizeAllSeparators(relativePath))) {
			// It may happen, that we cannot create a uniform path with the
			// given relative path (e.g. if the relative path will be above the
			// root of the uniform path).
			// To prevent these exceptions beforehand we check relative paths
			// against the exclude patterns. We may not use the include patterns
			// as these are aligned with uniform paths and using them can result
			// in all relative paths being excluded.
			excludedPaths.add(relativePath);
			return null;
		}

		try {
			String uniformPath = scopeDescriptor
					.createRelativeUniformPath(relativePath);
			if (!isIncluded(UniformPathUtils.stripProject(uniformPath))) {
				excludedPaths.add(uniformPath);
				return null;
			}

			return scopeDescriptor.createRelativeAccessor(relativePath);
		} catch (ConQATException e) {
			getLogger()
					.log(logLevel,
							"Could not access: " + relativePath + ": "
									+ e.getMessage());
			return null;
		}
	}

	/** Log included accessors */
	private void logIncludedElements(
			Collection<IContentAccessor> includedElements) {
		Set<String> uniformPaths = new HashSet<String>();
		for (IContentAccessor accessor : includedElements) {
			uniformPaths.add(accessor.getUniformPath());
		}

		logElements(uniformPaths, true);
	}

	/** Log excluded paths */
	private void logExcludedElements(Collection<String> excludedElements) {
		logElements(excludedElements, false);
	}

	/** Logs a collection of paths either as included or excluded log message. */
	private void logElements(Collection<String> elements, boolean included) {
		getLogger().info(
				new IncludeExcludeListLogMessage("files", included, elements,
						StructuredLogTags.SCOPE, StructuredLogTags.FILES));
	}

}