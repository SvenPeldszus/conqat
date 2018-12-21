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
package org.conqat.engine.resource.scope.zip;

/**
 * Common representation for paths to entries inside a Zip file. Those paths
 * have two parts: The path to the Zip file and the path inside the Zip file to
 * the entry.
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 47934 $
 * @ConQAT.Rating GREEN Hash: 258422E0C068564030BD1E3A30B0F370
 */
public class ZipPath {

	/**
	 * Separator string used between the zip file name and the zip entry. This
	 * is consistent with ZIP file URLs in Java.
	 */
	public static final String ZIP_FILE_SEPARATOR = "!";

	/** The path to a Zip file. */
	private String zipFilename;

	/** The path inside the Zip file to a entry. */
	private String zipEntryName;

	/**
	 * Creates a zip path from a single path containing an
	 * {@link ZipPath#ZIP_FILE_SEPARATOR}. If no separator is found, the entry
	 * path is null.
	 */
	public ZipPath(String path) {
		if (path.contains(ZIP_FILE_SEPARATOR)) {
			String[] parts = path.split(ZIP_FILE_SEPARATOR, 2);
			zipFilename = parts[0];
			zipEntryName = parts[1];
		} else {
			zipFilename = path;
			zipEntryName = null;
		}
	}

	/** Constructs a ZipPath from a filename and an entry path. */
	public ZipPath(String zipFilename, String zipEntryName) {
		this.zipFilename = zipFilename;
		this.zipEntryName = zipEntryName;
	}

	/** The filesystem path to a Zip file. */
	public String getZipFilename() {
		return zipFilename;
	}

	/** The path inside a Zip file to an entry. */
	public String getEntryName() {
		return zipEntryName;
	}

	/** Returns true if the path contains an entry to a file inside the Zip. */
	public boolean isValid() {
		return zipEntryName != null;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getPath();
	}

	/**
	 * Returns the full path to the entry, including the path to the Zip file,
	 * separated by {@link ZipPath#ZIP_FILE_SEPARATOR}
	 */
	public String getPath() {
		if (!isValid()) {
			return zipFilename;
		}
		return zipFilename + ZIP_FILE_SEPARATOR + zipEntryName;
	}
}