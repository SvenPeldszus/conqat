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
package org.conqat.engine.io.csv;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.enums.EnumUtils;

/**
 * A class for parsing CSV based on a provided enumeration containing the column
 * headers. The reader is iterable row by row, and each value can be requested
 * based on the column's enum value.
 * 
 * Column headers are normalized before being mapped to the enum values. See
 * {@link #normalizeHeader(String)} for a description of the normalization.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51666 $
 * @ConQAT.Rating GREEN Hash: A0C31D070D42C167762C3C22E6E4C523
 */
public class CSVEnumParser<E extends Enum<E>> extends
		CSVParserBase<EnumMap<E, String>> {

	/** The enum class describing the column titles */
	private final Class<E> enumClass;

	/** A subset of the columns to be contained in the result */
	private final EnumSet<E> columnsToRead;

	/**
	 * The enum values in the order they appeared in the header of the CSV file.
	 */
	private final List<E> headerEnums = new ArrayList<>();

	/**
	 * Constructor that retrieves only the given columns.
	 */
	public CSVEnumParser(Class<E> clazz, EnumSet<E> columnsToRead,
			Configuration configuration) {
		super(configuration);
		this.enumClass = clazz;
		this.columnsToRead = columnsToRead;
	}

	/**
	 * Constructor that retrieves all columns.
	 */
	public CSVEnumParser(Class<E> clazz, Configuration configuration) {
		this(clazz, EnumSet.allOf(clazz), configuration);
	}

	/** {@inheritDoc} */
	@Override
	protected void processHeaderRow(String[] headerRow) throws ConQATException {
		for (String header : headerRow) {
			String normalizedHeader = normalizeHeader(header);
			E enumConstant = EnumUtils.valueOf(enumClass, normalizedHeader);
			if (enumConstant == null) {
				throw new ConQATException("Could not map header cell '"
						+ header + "' to an enum constant");
			}
			headerEnums.add(enumConstant);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected EnumMap<E, String> mapRow(List<String> row) {
		CCSMAssert.isTrue(row.size() == headerEnums.size(),
				"Row has more or less entries than header: row = " + row.size()
						+ ", header = " + headerEnums.size());
		EnumMap<E, String> map = new EnumMap<>(enumClass);
		for (int i = 0; i < row.size(); i++) {
			E enumConstant = headerEnums.get(i);
			if (columnsToRead.contains(enumConstant)) {
				map.put(enumConstant, row.get(i));
			}
		}
		return map;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Uppercases the header and replaces any consecutive non-alphanumeric
	 * characters with an underscore.
	 */
	@Override
	protected String normalizeHeader(String header) {
		return header.toUpperCase().replaceAll("[^A-Z0-9]+", "_");
	}

}
