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

import java.util.regex.Pattern;

import org.conqat.lib.commons.filesystem.AntPatternUtils;

/**
 * Combines a list of include and exclude patterns. The patterns are specified
 * as an ANT patterns and then converted to Java Regex patterns.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51617 $
 * @ConQAT.Rating GREEN Hash: 2F75ECF95123A786C9A9BFD4A75BBC83
 */
public class IncludeExcludeAntPatternSupport extends
		IncludeExcludePatternSupportBase {

	/**
	 * Whether the provided ANT patterns are treated as case-sensitive.
	 */
	private boolean caseSensitive;

	/**
	 * Constructs a new case-insensitive {@link IncludeExcludeAntPatternSupport}
	 */
	public IncludeExcludeAntPatternSupport() {
		this(false);
	}

	/** Constructs a new {@link IncludeExcludeAntPatternSupport} */
	public IncludeExcludeAntPatternSupport(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	/**
	 * Sets case sensitivity. This method may not be called after the first call
	 * to {@link #isIncluded(String)}.
	 * 
	 * @see #caseSensitive
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		if (patternsCompiled()) {
			throw new IllegalStateException(
					"Must not be called after isIncluded has been used (lazy compilation of patterns).");
		}
		this.caseSensitive = caseSensitive;
	}

	/** {@inheritDoc} */
	@Override
	public Pattern compileToRegexPattern(String pattern) {
		return AntPatternUtils.convertPattern(pattern, caseSensitive);
	}
}
