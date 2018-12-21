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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.conqat.engine.core.bundle.BundleInfo;
import org.conqat.engine.core.driver.runner.ConQATRunnableBase;
import org.conqat.lib.commons.options.AOption;

/**
 * Base class for runners that support selecting bundles.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47370 $
 * @ConQAT.Rating GREEN Hash: 3EF3E3505D0607B0DCB61931AD002EDA
 */
public abstract class BundleExcludingRunnerBase extends ConQATRunnableBase {

	/** Patterns used for bundle exclusion. */
	private final List<Pattern> excludePatterns = new ArrayList<>();

	/** Sets exclude patterns. */
	@AOption(shortName = 'x', longName = "exclude", description = "Sets a regex pattern used to exclude bundles.")
	public void addExcludePattern(String regex) {
		excludePatterns.add(Pattern.compile(regex));
	}

	/** Returns included bundles. */
	protected Set<BundleInfo> getIncludedBundles() {
		Set<BundleInfo> bundles = new HashSet<>();
		for (BundleInfo bundle : bundleConfig.getBundles()) {
			if (isIncluded(bundle)) {
				bundles.add(bundle);
			}
		}
		return bundles;
	}

	/** Returns whether the bundle should be included. */
	private boolean isIncluded(BundleInfo bundle) {
		String id = bundle.getId();
		for (Pattern excludePattern : excludePatterns) {
			if (excludePattern.matcher(id).matches()) {
				return false;
			}
		}
		return true;
	}

}
