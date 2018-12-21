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
package org.conqat.engine.resource.filters;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.commons.ConQATPipelineProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 49331 $
 * @ConQAT.Rating GREEN Hash: 1734CAEB14EA58FCCF4C0D6DD8F93208
 */
@AConQATProcessor(description = "This processor compares an unfiltered scope with a filtered "
		+ "one and writes all filtered elements' uniform paths into a file for use with the "
		+ "CachedElementFilter processor.")
public class ElementFilterCacheUpdater extends
		ConQATPipelineProcessorBase<IResource> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "blacklist-file", attribute = "path", description = ""
			+ "Path to the filter blacklist cache file. Each line of this file will contain "
			+ "the uniform path of an element that was filtered.", optional = false)
	public String blacklistPath;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "unfiltered", attribute = "scope", description = ""
			+ "Input scope before filtering", optional = false)
	public IResource unfilteredScope;

	/** {@inheritDoc} */
	@Override
	protected void processInput(IResource input) {
		Set<String> currentElements = collectPaths(input);
		Set<String> priorElements = collectPaths(unfilteredScope);
		priorElements.removeAll(currentElements);

		writeFile(priorElements, blacklistPath);
	}

	/** Retrieves the uniform paths for all elements in the input list. */
	private static Set<String> collectPaths(IResource root) {
		Set<String> result = new HashSet<>();
		for (IElement element : ResourceTraversalUtils.listElements(root)) {
			result.add(element.getUniformPath());
		}
		return result;
	}

	/**
	 * Writes strings to a file, separated by newline characters.
	 */
	private void writeFile(Set<String> contents, String outputPath) {
		try {
			FileSystemUtils.writeFileUTF8(new File(outputPath), StringUtils
					.concat(CollectionUtils.sort(contents), StringUtils.CR));
		} catch (IOException e) {
			getLogger().error("File could not be written: " + outputPath, e);
		}
	}
}