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
package org.conqat.engine.core.bundle.library;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.core.bundle.BundleException;
import org.conqat.engine.core.bundle.BundleInfo;
import org.conqat.engine.core.bundle.BundlesLoader;
import org.conqat.lib.commons.options.AOption;

/**
 * Base class for actions regarding analysis of bundle libraries and
 * descriptors.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47370 $
 * @ConQAT.Rating GREEN Hash: 161929B29437F93D1D4F80B3C18F040C
 */
public abstract class LibraryDescriptorRunnerBase extends
		BundleExcludingRunnerBase {

	/** The output stream to print results to. */
	private PrintStream outputStream = System.out;

	/** Artificial bundle info for the core bundle. */
	private BundleInfo coreBundleInfo;

	/**
	 * Changes the output stream to the given file.
	 */
	@AOption(shortName = 'o', longName = "out", description = "Prints the output into the given file instead of standard output.")
	public void setOutputFile(String fileName) throws IOException {
		outputStream = new PrintStream(new File(fileName));
	}

	/** {@inheritDoc} */
	@Override
	protected final void doRun() {
		coreBundleInfo = createCoreBundleInfo();
		coreBundleInfo.setName("ConQAT Core");
		try {
			BundlesLoader.loadLibraryLocations(coreBundleInfo);
		} catch (BundleException e) {
			throw new AssertionError("Could not load core bundle!", e);
		}

		doRun(outputStream);
		if (outputStream != System.out) {
			outputStream.close();
		}
	}

	/** Performs the actual execution and prints results to the given stream. */
	abstract void doRun(PrintStream out);

	/** Returns the library directory of the core bundle. */
	private BundleInfo createCoreBundleInfo() {
		try {
			for (BundleInfo bundle : bundleConfig.getBundles()) {
				File coreLib = new File(bundle.getLocation().getParentFile(),
						"org.conqat.engine.core/lib");
				if (coreLib.isDirectory()) {
					return new BundleInfo(coreLib.getParentFile());
				}

				if (bundle.getLocation().getParentFile().getName()
						.equals("bundles")) {
					File libDir = new File(bundle.getLocation().getParentFile()
							.getParentFile(), BundlesLoader.LIB_LOCATION);
					if (libDir.isDirectory()) {
						return new BundleInfo(libDir.getParentFile());
					}
				}
			}
		} catch (BundleException e) {
			throw new AssertionError("Could not create core bundle!", e);
		}

		throw new AssertionError(
				"Expected this to be a valid ConQAT installation but could not find core bundle!");
	}

	/** @return The a list of all loaded java libraries. */
	protected Set<File> getLibraryFiles() {
		Set<File> libraries = new HashSet<File>();
		for (BundleInfo bundle : getIncludedBundles()) {
			libraries.addAll(bundle.getLibraries());
		}
		libraries.addAll(coreBundleInfo.getLibraries());
		return libraries;
	}

	/**
	 * @return The library descriptors for all loaded bundles.
	 */
	protected Set<LibraryDescriptor> getLibraryDescriptors() {
		Set<LibraryDescriptor> descriptors = new HashSet<LibraryDescriptor>();
		for (BundleInfo bundle : getIncludedBundles()) {
			descriptors.addAll(bundle.getLibraryDescriptors());
		}
		descriptors.addAll(coreBundleInfo.getLibraryDescriptors());

		return descriptors;
	}
}
