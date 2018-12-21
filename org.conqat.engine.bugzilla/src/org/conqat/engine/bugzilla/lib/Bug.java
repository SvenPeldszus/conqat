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
package org.conqat.engine.bugzilla.lib;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableSet;
import org.conqat.lib.commons.string.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import com.mdimension.jchronic.Chronic;

/**
 * This class represents a Bug stored in Bugzilla. The values of the various
 * Bugzilla fields are stored in a map that maps from {@link EBugzillaField} to
 * the corresponding value. Hence, a Bug is not guaranteed to store all fields.
 * We chose this design to make it possible to iterate over the fields stored in
 * a bug. Moreover, it is more flexible than using dedicated fields with getters
 * and setters as new fields can be added by simply adding a new enumeration
 * element to {@link EBugzillaField}.
 * 
 * @author $Author:deissenb $
 * @version $Revision: 47707 $
 * @ConQAT.Rating GREEN Hash: E0F8FA0E5E8776F34CBAE854412CFA36
 */
public class Bug {

	/** Bug id. */
	private final int id;

	/** Maps from Bugzilla field to value. */
	private final Map<EBugzillaField, String> fields = new EnumMap<EBugzillaField, String>(
			EBugzillaField.class);

	/** Maps from custom field to value. */
	private final Map<String, String> customFields = new HashMap<String, String>();

	/** Pattern that matches dates saved today */
	private static Pattern TODAY_PATTERN = Pattern
			.compile("[0-9]{2}:[0-9]{2}:[0-9]{2}");

	/** Pattern that matches dates saved during the last week */
	private static Pattern LAST_WEEK_PATTERN = Pattern
			.compile("[A-Z][a-z][a-z] [0-9]{2}:[0-9]{2}");

	/** Pattern that matches any other dates */
	private static Pattern ANY_DATE_PATTERN = Pattern
			.compile("[0-9]{4}-[0-9]{2}-[0-9]{2}");

	/** Date formatter for normal dates */
	private static DateTimeFormatter DATE_FORMATTER = DateTimeFormat
			.forPattern("yyyy-MM-dd");

	/** Date formatter that formats a given time */
	private static DateTimeFormatter TIME_FORMATTER = DateTimeFormat
			.forPattern("HH:mm:ss");

	/** Create new Bug. */
	public Bug(int id) {
		this.id = id;
	}

	/** Get bug id. */
	public int getId() {
		return id;
	}

	/** Set a field value. */
	public void setField(EBugzillaField field, String value) {
		fields.put(field, value);
	}

	/** Get field value. Returns <code>null</code> if field is undefined. */
	public String getValue(EBugzillaField field) {
		return fields.get(field);
	}

	/** Get all fields defined for this bug. */
	public UnmodifiableSet<EBugzillaField> getFields() {
		return CollectionUtils.asUnmodifiable(fields.keySet());
	}

	/** Get string representation that includes all field values. */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();

		result.append("Bug " + id + " [" + StringUtils.CR);
		result.append(StringUtils.toString(fields, "  ") + StringUtils.CR);
		if (!customFields.isEmpty()) {
			result.append(StringUtils.toString(customFields, "  ")
					+ StringUtils.CR);
		}
		result.append("]");

		return result.toString();
	}

	/** Set a custom field value. */
	public void setCustomField(String field, String value) {
		customFields.put(field, value);
	}

	/** Get custom field value. Returns <code>null</code> if field is undefined. */
	public String getCustomFieldValue(String field) {
		return customFields.get(field);
	}

	/** Get names of all custom fields defined for this bug. */
	public UnmodifiableSet<String> getCustomFields() {
		return CollectionUtils.asUnmodifiable(customFields.keySet());
	}

	/** Get milliseconds of an enumeration field that is holding a date. */
	public long getMilliSeconds(EBugzillaField field) throws ConQATException {
		if (fields.get(field) == null) {
			throw new ConQATException("Could not find Bugzilla field: " + field);
		}

		String bugzillaDate = fields.get(field);
		if (ANY_DATE_PATTERN.matcher(bugzillaDate).matches()) {
			return DATE_FORMATTER.parseDateTime(bugzillaDate).getMillis();
		} else if (LAST_WEEK_PATTERN.matcher(bugzillaDate).matches()) {
			DateTime lastWeekDate = new DateTime(Chronic.parse(bugzillaDate)
					.getBeginCalendar().getTime());
			// Since jchronic parses the Bugzilla format exactly seven days
			// to late, we need to subtract those 7 days.
			return lastWeekDate.minusDays(7).getMillis();
		} else if (TODAY_PATTERN.matcher(bugzillaDate).matches()) {
			DateTime todayDate = new DateTime();
			DateTime fieldDate = TIME_FORMATTER.parseDateTime(bugzillaDate);
			return new DateTime(todayDate.getYear(),
					todayDate.getMonthOfYear(), todayDate.getDayOfMonth(),
					fieldDate.getHourOfDay(), fieldDate.getMinuteOfHour(),
					fieldDate.getSecondOfMinute()).getMillis();
		}
		throw new ConQATException("Data in field '" + field
				+ "' is not a Bugzilla date.");
	}
}