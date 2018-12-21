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

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.supercsv.comment.CommentMatches;
import org.supercsv.io.CsvListReader;
import org.supercsv.prefs.CsvPreference;

/**
 * Base class for CSV parsers. Subclasses may define the final representation of
 * each parsed row, e.g. as a list, map, nodes, etc.
 * 
 * This class uses the SuperCSV library, which can handle many different CSV
 * styles reliably.
 * 
 * Note that since the CSV library may throw {@link IOException}s during
 * parsing, we did not implement the iterator in a lazy way (i.e. parse the next
 * row when the user requests more data). Instead the entire file is parsed at
 * construction time and cached.
 * 
 * This parser cannot be reused and is not thread safe.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51653 $
 * @ConQAT.Rating GREEN Hash: BFC1437B4C059B01354055B2950B8E21
 */
public abstract class CSVParserBase<RowType> implements Iterable<RowType> {

	/** The parsed CSV data mapped to the row type. */
	private List<RowType> data = null;

	/** The configuration of the parser. */
	private final Configuration configuration;

	/**
	 * Constructor.
	 */
	public CSVParserBase(Configuration configuration) {
		this.configuration = configuration;
	}

	/**
	 * Parses the data from the given reader and wraps all thrown exceptions in
	 * a {@link ConQATException}.
	 * 
	 * This method may only be called once per reader. Afterwards, the
	 * {@link #iterator()} may be used to iterate over the parsed data.
	 */
	public void parseAndWrapExceptions(Reader inputReader)
			throws ConQATException {
		CCSMAssert.isTrue(data == null, "You may not reuse this parser");
		data = new ArrayList<>();

		try {
			parse(inputReader);
		} catch (IOException e) {
			throw new ConQATException(
					"An IO error occurred while parsing the CSV file", e);
		}
	}

	/**
	 * Parses the data from the given reader and stores it in {@link #data}.
	 */
	private void parse(Reader inputReader) throws IOException, ConQATException {
		try (CsvListReader reader = new CsvListReader(inputReader,
				configuration.createPreference())) {
			String[] header = reader.getHeader(true);
			if (header == null) {
				if (configuration.allowEmptyFiles) {
					return;
				}
				throw new ConQATException(
						"The CSV file did not contain a header row.");
			}
			processHeaderRow(header);

			List<String> row = readNextRow(reader);
			while (row != null) {
				data.add(mapRow(row));
				row = readNextRow(reader);
			}
		}
	}

	/**
	 * Reads the next row, converting <code>null</code> values to the empty
	 * string, if the configuration requested it.
	 * 
	 * @return the processed row or <code>null</code> if the reader is finished.
	 */
	private List<String> readNextRow(CsvListReader reader) throws IOException {
		List<String> row = reader.read();
		if (row == null || !configuration.convertNullToEmptyString) {
			return row;
		}

		List<String> convertedRow = new ArrayList<>();
		for (String value : row) {
			if (value == null) {
				value = StringUtils.EMPTY_STRING;
			}
			convertedRow.add(value);
		}
		return convertedRow;
	}

	/**
	 * May be overwritten by subclasses to perform some processing on the header
	 * row, e.g. store it for later use or transform it. The default
	 * implementation does nothing.
	 * 
	 * @param header
	 *            the parsed header row.
	 * @throws ConQATException
	 *             if processing the header row failed
	 */
	protected void processHeaderRow(String[] header) throws ConQATException {
		// default implementation does nothing
	}

	/**
	 * Maps the given row to the row type.
	 */
	protected abstract RowType mapRow(List<String> row);

	/**
	 * Normalizes the given header cell content so it can be mapped to an
	 * enumeration value. May be overwritten by subclasses to customize the
	 * normalization. This default implementation does nothing.
	 */
	protected String normalizeHeader(String header) {
		return header;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * The returned iterator does not support removal.
	 */
	@Override
	public Iterator<RowType> iterator() {
		assertDataWasParsed();
		return CollectionUtils.asUnmodifiable(data).iterator();
	}

	/**
	 * Asserts that the user has called {@link #parseAndWrapExceptions(Reader)}.
	 */
	protected void assertDataWasParsed() {
		CCSMAssert.isNotNull(data,
				"You must call #parseAndWrapExceptions() before "
						+ "you can use this method");
	}

	/** Returns the number of rows parsed. */
	public int getRowCount() {
		assertDataWasParsed();
		return data.size();
	}

	/**
	 * This class wraps the {@link CsvPreference} class so that users of this
	 * parser don't have to directly reference the SuperCSV library.
	 * 
	 * This configuration is already filled with sane defaults, see
	 * {@link #quoteChar}, {@link #delimiterChar} and {@link #endOfLineSymbols}
	 */
	public static class Configuration {

		/** The quote character. Defaults to a double quote. */
		private char quoteChar = '"';

		/** The column delimiter character. Defaults to a semicolon. */
		private char delimiterChar = ';';

		/** All possible end of line characters. Defaults to "\r\n". */
		private String endOfLineSymbols = "\r\n";

		/**
		 * The regular expression that identifies comment lines that should be
		 * ignored or <code>null</code> in case the file does not contain
		 * comment lines. The default is <code>null</code>.
		 * 
		 * Note that the pattern should be efficient, i.e. it should ideally
		 * match the start of the line ("^"). Otherwise, parser performance will
		 * degrade.
		 */
		private String commentLinePattern = null;

		/**
		 * If this is <code>true</code>, empty cells in the CSV file are
		 * represented as the empty string. Otherwise, they are represented as
		 * <code>null</code>. Defaults to <code>true</code>.
		 */
		protected boolean convertNullToEmptyString = true;

		/**
		 * If this is <code>true</code>, completely empty CSV files (even
		 * without header line) are allowed. Otherwise, an empty file causes an
		 * Exception. Defaults to <code>false</code>.
		 */
		protected boolean allowEmptyFiles = false;

		/**
		 * Sets {@link #quoteChar}. Returns this configuration to allow
		 * chaining.
		 */
		public Configuration setQuoteChar(char quoteChar) {
			this.quoteChar = quoteChar;
			return this;
		}

		/**
		 * Sets {@link #convertNullToEmptyString}. Returns this configuration to
		 * allow chaining.
		 */
		public Configuration setConvertNullToEmptyString(
				boolean convertNullToEmptyString) {
			this.convertNullToEmptyString = convertNullToEmptyString;
			return this;
		}

		/**
		 * Sets {@link #allowEmptyFiles}. Returns this configuration to allow
		 * chaining.
		 */
		public Configuration setAllowEmptyFiles(boolean allowEmptyFiles) {
			this.allowEmptyFiles = allowEmptyFiles;
			return this;
		}

		/**
		 * Sets {@link #delimiterChar}. Returns this configuration to allow
		 * chaining.
		 */
		public Configuration setDelimiterChar(char delimiterChar) {
			this.delimiterChar = delimiterChar;
			return this;
		}

		/**
		 * Sets {@link #commentLinePattern}, a regular expression. Returns this
		 * configuration to allow chaining.
		 */
		public Configuration setCommentLinePattern(String commentLinePattern) {
			this.commentLinePattern = commentLinePattern;
			return this;
		}

		/**
		 * Sets {@link #endOfLineSymbols}. Returns this configuration to allow
		 * chaining.
		 */
		public Configuration setEndOfLineSymbols(String endOfLineSymbols) {
			this.endOfLineSymbols = endOfLineSymbols;
			return this;
		}

		/** Builds a preference object from this configuration's data. */
		private CsvPreference createPreference() {
			CsvPreference.Builder builder = new CsvPreference.Builder(
					quoteChar, delimiterChar, endOfLineSymbols);
			if (commentLinePattern != null) {
				builder.skipComments(new CommentMatches(commentLinePattern));
			}
			return builder.build();
		}

	}

}
