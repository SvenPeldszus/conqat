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
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.ArrayList;
import java.util.List;

/**
 * A configuration of boolean values for a multi-condition. This has 2^n
 * configuration for a list of n sub conditions.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51097 $
 * @ConQAT.Rating GREEN Hash: 8590118A1301D8663DBCF74CF19B0A53
 */
public class Configuration {

	/** The configuration's boolean values. */
	private final List<Boolean> configuration;

	/** Creates a new empty configuration. */
	private Configuration() {
		configuration = new ArrayList<>();
	}

	/** Copies this configuration and adds the given value to the copy. */
	private Configuration(Configuration other, boolean appendedValue) {
		configuration = new ArrayList<>(other.configuration);
		addValue(appendedValue);
	}

	/** Adds a value and returns this configuration. */
	private Configuration addValue(boolean value) {
		configuration.add(value);
		return this;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return toMaskedString(-1);
	}

	/** Returns a list of all possible configurations of the given length. */
	public static List<Configuration> generateConfigurations(int length) {
		List<Configuration> configurations = new ArrayList<>();
		if (length <= 0) {
			configurations.add(new Configuration());
		} else {
			for (Configuration subConfiguration : generateConfigurations(length - 1)) {
				configurations.add(new Configuration(subConfiguration, false));
				configurations.add(subConfiguration.addValue(true));
			}
		}
		return configurations;
	}

	/**
	 * Returns a string representation with the value at given index replaced by
	 * "X".
	 */
	public String toMaskedString(int index) {
		if (configuration.isEmpty()) {
			return "[]";
		}
		StringBuilder builder = new StringBuilder("[");
		for (int i = 0; i < configuration.size(); i++) {
			if (i > 0) {
				builder.append(",");
			}

			if (i == index) {
				builder.append("X");
			} else {
				builder.append(configuration.get(i));
			}
		}
		builder.append("]");
		return builder.toString();
	}

	/** Returns the i-th element of this configuration. */
	public boolean get(int index) {
		return configuration.get(index);
	}
}
