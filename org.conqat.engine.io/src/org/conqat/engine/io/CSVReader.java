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
package org.conqat.engine.io;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.util.List;

import org.conqat.engine.commons.CommonUtils;
import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.engine.commons.node.ListNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.io.csv.CSVParserConfiguration;
import org.conqat.engine.io.csv.CsvStringListParser;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.reflect.ReflectionUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author juergens
 * @author $Author: goeb $
 * @version $Rev: 51578 $
 * @ConQAT.Rating GREEN Hash: 8A22BD575452F1FDDB1CDBC8AF3AEF68
 */
@AConQATProcessor(description = "Reads CSV (comma-separated value) files into a rooted list of ConQAT nodes. "
		+ "For each line in the CSV file, a single {@link IConQATNode} is created. A "
		+ "dedicated column of the CSV file serves as node ids. "
		+ "The first line in the file is expected to contain column names.")
public class CSVReader extends ConQATProcessorBase {

	/** CSV file name */
	private String filename;

	/** Name of the column that is used for IDs */
	private String idColumn;

	/** Converts value strings into typed objects */
	private final TypeConverter converter = new TypeConverter();

	/** Encoding for file read. */
	private Charset encoding = Charset.defaultCharset();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "csv", minOccurrences = 1, maxOccurrences = 1, description = ""
			+ "Comma-separated value file that gets read")
	public void setFile(
			@AConQATAttribute(name = "file", description = "Name of the file") String filename,
			@AConQATAttribute(name = "idColumn", description = "Name of the column from which node ids are taken") String idColumn) {
		this.filename = filename;
		this.idColumn = idColumn;
	}

	/** Contains the CSV configuration. */
	@AConQATParameterObject
	public CSVParserConfiguration parserConfiguration = new CSVParserConfiguration();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "type", description = "Set the type for a column")
	public void addType(
			@AConQATAttribute(name = "column", description = "Name of the column") String columnName,
			@AConQATAttribute(name = "typename", description = "Name of the type. Allowed values are all java primitives. (int, boolean, long, ...)") String typeName) {

		try {
			Class<?> clazz = ReflectionUtils.resolveType(typeName);
			converter.addTypeAssociation(columnName, clazz);
		} catch (ClassNotFoundException e) {
			getLogger().error("Could not find type: " + typeName);
		}
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = ConQATParamDoc.ENCODING_PARAM_NAME, minOccurrences = 0, maxOccurrences = 1, description = ConQATParamDoc.ENCODING_PARAM_DESC)
	public void setEncoding(
			@AConQATAttribute(name = ConQATParamDoc.ENCODING_ATTR_NAME, description = ConQATParamDoc.ENCODING_ATTR_DESC) String encodingName)
			throws ConQATException {
		encoding = CommonUtils.obtainEncoding(encodingName);
	}

	/** {@inheritDoc} */
	@Override
	public IRemovableConQATNode process() throws ConQATException {
		CsvStringListParser parser = new CsvStringListParser(
				parserConfiguration.getConfiguration());
		try {
			parser.parseAndWrapExceptions(new StringReader(FileSystemUtils
					.readFile(new File(filename), encoding.name())));
		} catch (IOException e) {
			throw new ConQATException("Could not read file " + filename + ": "
					+ e.getMessage(), e);
		}

		List<String> columnNames = parser.getHeader();
		if (!columnNames.contains(idColumn)) {
			throw new ConQATException("Id column '" + idColumn
					+ "' not found in header of file " + filename);
		}

		ListNode listRoot = createNodeList(columnNames, parser);
		NodeUtils.addToDisplayList(listRoot, columnNames);
		return listRoot;
	}

	/**
	 * Creates a rooted list containing a {@link ListNode} for each line
	 * (excluding the header line)
	 */
	private ListNode createNodeList(List<String> columnNames,
			CsvStringListParser parser) throws ConQATException {
		ListNode listRoot = new ListNode();

		int idColumnIndex = columnNames.indexOf(idColumn);
		for (List<String> values : parser) {
			ListNode node = new ListNode(values.get(idColumnIndex));

			for (int columnIndex = 0; columnIndex < values.size(); columnIndex++) {
				if (columnIndex != idColumnIndex) {
					String columnName = columnNames.get(columnIndex);
					Object typedValue = converter.typedValueFor(columnName,
							values.get(columnIndex));
					node.setValue(columnName, typedValue);
				}
			}

			listRoot.addChild(node);
		}
		return listRoot;
	}

}