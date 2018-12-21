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
package org.conqat.engine.commons.pattern;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Combines a list of include and exclude patterns. The patterns are specified
 * as strings. Implementing classes define the semantics they are treated
 * according to by implementing {@link #compileToRegexPattern(String)} to
 * compile the pattern strings to Java Regex patterns.
 * 
 * Patterns are compiled lazily the first time {@link #isIncluded(String)} is
 * called. Consequently, after this method has been called, no further patterns
 * can be added.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51297 $
 * @ConQAT.Rating GREEN Hash: C3861C3AC1BC8E5DCBE7C723088A7291
 */
public abstract class IncludeExcludePatternSupportBase {

	/** The include patterns in their string representation. */
	private final List<String> includes = new ArrayList<String>();

	/** The exclude patterns in their string representation. */
	private final List<String> excludes = new ArrayList<String>();

	/** The compiled include patterns */
	private PatternList includePatterns = null;

	/** The compiled exclude patterns */
	private PatternList excludePatterns = null;

	/** Returns a String representation of the include patterns. */
	public List<String> getIncludePatterns() {
		return includes;
	}

	/** Returns a String representation of the exclude patterns. */
	public List<String> getExcludePatterns() {
		return excludes;
	}

	/** Whether the provided patterns have already been compiled. */
	protected boolean patternsCompiled() {
		return includePatterns != null;
	}

	/** Adds the given include pattern. */
	public void addIncludePattern(String include) {
		includes.add(include);
	}

	/** Adds the given exclude pattern. */
	public void addExcludePattern(String exclude) {
		excludes.add(exclude);
	}

	/** Returns whether the given name should be included. */
	public boolean isIncluded(String name) {
		if (includePatterns == null) {
			includePatterns = new PatternList();
			excludePatterns = new PatternList();
			for (String include : includes) {
				includePatterns.add(compileToRegexPattern(include));
			}
			for (String exclude : excludes) {
				excludePatterns.add(compileToRegexPattern(exclude));
			}
		}
		return (includePatterns.emptyOrMatchesAny(name))
				&& !excludePatterns.matchesAny(name);
	}

	/** Converts provided pattern strings to Java RegEx patterns. */
	public abstract Pattern compileToRegexPattern(String pattern);
}
