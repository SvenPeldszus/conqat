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
import java.util.Collection;
import java.util.List;

import org.conqat.engine.sourcecode.coverage.volume.LineHint;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.string.StringUtils;

/**
 * A condition that consists of multiple sub-conditions.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51105 $
 * @ConQAT.Rating GREEN Hash: 711128A0F9727F407DF96A988ACFF8F7
 */
public class MultiCondition {

	/** The internal sub-conditions. */
	private final List<Condition> subConditions;

	/** All possible configurations of the internal sub-conditions. */
	private final List<Configuration> configurations;

	/** Creates a new multi-condition with the given sub-conditions. */
	public MultiCondition(List<Condition> subConditions) {
		CCSMPre.isNotNull(subConditions);
		this.subConditions = subConditions;
		this.configurations = Configuration
				.generateConfigurations(subConditions.size());
	}

	/**
	 * Returns a list of {@link LineHint}s representing the condition with all
	 * possible configurations.
	 */
	public Collection<LineHint> createHintList() {
		return toHintList(subConditions, configurations);
	}

	/**
	 * Returns a list of {@link LineHint}s representing the condition with all
	 * possible configurations.
	 */
	public static Collection<LineHint> toHintList(
			List<Condition> subConditions, List<Configuration> configurations) {
		List<LineHint> hints = new ArrayList<>();
		String subConditionString = toSubConditionString(subConditions) + " ";

		int lineNumber = -1;
		if (!subConditions.isEmpty()) {
			lineNumber = subConditions.get(0).getLineNumber();
		}
		for (Configuration configuration : configurations) {
			hints.add(new LineHint(subConditionString + " " + configuration
					+ " (" + lineNumber + ")", lineNumber));
		}
		return hints;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return toSubConditionString(subConditions);
	}

	/** Returns a string representation of the given sub conditions. */
	private static String toSubConditionString(List<Condition> subConditions) {
		if (subConditions.isEmpty()) {
			return StringUtils.EMPTY_STRING;
		}

		StringBuilder builder = new StringBuilder("[");
		builder.append(subConditions.get(0).getText());
		for (int i = 1; i < subConditions.size(); i++) {
			builder.append(",");
			builder.append(subConditions.get(i).getText());
		}
		builder.append("]");
		return builder.toString();
	}

	/** @see #configurations */
	public UnmodifiableList<Configuration> getConfigurations() {
		return CollectionUtils.asUnmodifiable(configurations);
	}
}
