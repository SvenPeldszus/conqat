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
package org.conqat.engine.core.bundle;

import java.io.File;

/**
 * This is the mandatory base class for bundle context classes. The context
 * class enables a bundle to obtain access to resources and bundle specific
 * information defined in the bundle descriptor.
 * <p>
 * Subclasses must be called <code>BundleContext</code> to be instantiated by
 * the bundle mechanism.
 *
 * @author Florian Deissenboeck
 * @author $Author: heinemann $
 * @version $Rev: 51198 $
 * @ConQAT.Rating GREEN Hash: 3210DC4D287C5C07AD3D2886BE608245
 */
public class BundleContextBase {

	/** The resource manager. */
	private final BundleResourceManager resourceManager;

	/** The bundle info. */
	private final BundleInfo bundleInfo;

	/** Create new bundle context. */
	protected BundleContextBase(BundleInfo bundleInfo) {
		if (bundleInfo == null) {
			throw new IllegalArgumentException("BundleInfo may not be null.");
		}
		resourceManager = new BundleResourceManager(bundleInfo);
		this.bundleInfo = bundleInfo;

	}

	/** Get the bundle's resource manager. */
	public BundleResourceManager getResourceManager() {
		return resourceManager;
	}

	/**
	 * Shortcut to get a resources as file from the current bundle.
	 *
	 * @see BundleResourceManager#getResourceAsFile(String)
	 */
	public File getResourceAsFile(String path) {
		return getResourceManager().getResourceAsFile(path);
	}

	/** Get bundle info (enables access to bundle descriptor information). */
	public BundleInfo getBundleInfo() {
		return bundleInfo;
	}
}