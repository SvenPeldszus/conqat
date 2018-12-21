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

import java.io.IOException;
import java.util.HashSet;

import org.conqat.engine.commons.filter.FilterBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 49331 $
 * @ConQAT.Rating GREEN Hash: E1A4F89371E4B8D9F60BE61DB29E77BF
 */
@AConQATProcessor(description = "This processor reads uniform paths from an input file and filters all elements "
		+ "listed in this file. This is helpful in case of expensive filter operations like content filters that "
		+ "would otherwise be executed over and over again. The list of files to filter can be updated via the "
		+ "ElementFilterCacheUpdater processor.")
public class CachedElementFilter extends FilterBase<IResource> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "blacklist-file", attribute = "path", description = ""
			+ "Path to the filter blacklist cache file. Each line of this file is treated "
			+ "as a uniform path of an element to be filtered.", optional = true)
	public String blacklistPath;

	/** The canonical paths of all elements to filter. */
	private final HashSet<String> filterPaths = new HashSet<>();

	/** {@inheritDoc} */
	@Override
	protected void preProcessInput(IResource input) throws ConQATException {
		super.preProcessInput(input);

		try {
			filterPaths.addAll(FileSystemUtils.readLinesUTF8(blacklistPath));
		} catch (IOException e) {
			getLogger().warn(
					"File could not be read. No elements were filtered.", e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFiltered(IResource node) {
		if (!(node instanceof IElement)) {
			return false;
		}
		return filterPaths.contains(((IElement) node).getUniformPath());
	}
}
