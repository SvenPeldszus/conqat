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
package org.conqat.engine.resource.scope.filesystem;

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.scope.ScopeBase;
import org.conqat.engine.resource.scope.zip.ZipEntryContentAccessor;
import org.conqat.engine.resource.scope.zip.ZipFileLogger;
import org.conqat.engine.resource.scope.zip.ZipPath;
import org.conqat.engine.resource.util.ConQATFileUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.lib.commons.filesystem.CanonicalFile;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 47947 $
 * @ConQAT.Rating GREEN Hash: F3CC2885333CD77AA377FD613CAC3141
 */
@AConQATProcessor(description = "Scope for a single file. This file can be within a ZIP file.")
public class SingleFileScope extends ScopeBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "file", attribute = "path", description = "Path to file. This can be a local path to a file or a file within a ZIP.")
	public String path;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(attribute = "ref", parameter = "zip-file-logger", optional = true, description = ""
			+ "If this parameter is set, all the files that are read in the zip are logged. This functionality is unused if the file is not within a ZIP.")
	public ZipFileLogger zipFileLogger = null;

	/** {@inheritDoc} */
	@Override
	protected IContentAccessor[] createAccessors() throws ConQATException {
		IContentAccessor accessor;

		ZipPath zipPath = new ZipPath(path);

		if (zipPath.isValid()) {
			CanonicalFile zipFile = ConQATFileUtils
					.createReadableCanonicalFile(zipPath.getZipFilename());
			String uniformPath = UniformPathUtils.concatenate(projectName,
					zipPath.getEntryName());
			accessor = new ZipEntryContentAccessor(zipFile,
					zipPath.getEntryName(), uniformPath, zipFileLogger);
		} else {
			CanonicalFile file = ConQATFileUtils
					.createReadableCanonicalFile(path);
			String uniformPath = UniformPathUtils.concatenate(projectName,
					file.getName());
			accessor = new FileContentAccessor(file, uniformPath);
		}

		getLogger().debug("Using file at location: " + accessor.getLocation());

		return new IContentAccessor[] { accessor };
	}
}