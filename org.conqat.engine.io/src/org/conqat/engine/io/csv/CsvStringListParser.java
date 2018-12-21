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

import java.util.Arrays;
import java.util.List;

/**
 * A class for parsing CSV which represents each row as a list of strings.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51572 $
 * @ConQAT.Rating GREEN Hash: 3AF6FBCCA88A13EECD3BF816780236CB
 */
public class CsvStringListParser extends CSVParserBase<List<String>> {

	/** The header row. */
	private List<String> header = null;

	/**
	 * Constructor.
	 */
	public CsvStringListParser(Configuration configuration) {
		super(configuration);
	}

	/** Returns the parsed header row. */
	public List<String> getHeader() {
		assertDataWasParsed();
		return header;
	}

	/** {@inheritDoc} */
	@Override
	protected void processHeaderRow(String[] header) {
		this.header = Arrays.asList(header);
	}

	/** {@inheritDoc} */
	@Override
	protected List<String> mapRow(List<String> row) {
		return row;
	}

}
