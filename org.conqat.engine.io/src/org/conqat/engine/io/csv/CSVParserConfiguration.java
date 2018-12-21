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

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.io.csv.CSVParserBase.Configuration;

/**
 * A ConQAT parameter object that contains all parameters necessary for a
 * {@link Configuration}.
 * 
 * The only thing that cannot be configured via this object are the end of line
 * characters, since these cannot be entered properly in the block/CQR editor.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51578 $
 * @ConQAT.Rating GREEN Hash: F32DE5443BF765B10D019D288FEEE4B2
 */
public class CSVParserConfiguration implements IConQATParameterObject {

	/** The configuration. */
	private final Configuration configuration = new Configuration();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "empty-cells", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Whether empty cells in the CSV are represented as the empty string (true) or null (false)")
	public void setConvertNullToEmptyString(
			@AConQATAttribute(name = "convert", description = "By default, empty columns are parsed as empty strings") boolean shouldConvert) {
		configuration.setConvertNullToEmptyString(shouldConvert);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "quote", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Character used to quote cells in the CSV")
	public void setQuoteCharacter(
			@AConQATAttribute(name = "character", description = "Default value is the double qoute") String character)
			throws ConQATException {
		char quoteChar = checkCharacterParameter(character, "quote");
		configuration.setDelimiterChar(quoteChar);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "comment", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Sets the pattern that identifies comment lines, which will be ignored. "
			+ "This pattern should be efficient, otherwise parsing performace will degrade.")
	public void setCommentPattern(
			@AConQATAttribute(name = "pattern", description = "By default, no lines are treated as comments") String pattern) {
		configuration.setCommentLinePattern(pattern);
	}

	/**
	 * Checks that the given parameter's value consists of exactly one character
	 * and returns that character. Otherwise, throws an exception.
	 */
	private char checkCharacterParameter(String value, String parameterName)
			throws ConQATException {
		if (value.length() != 1) {
			throw new ConQATException("The " + parameterName
					+ " parameter must consist of exactly one character.");
		}
		return value.charAt(0);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "separator", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Character used to split columns.")
	public void setSeparator(
			@AConQATAttribute(name = "separator", defaultValue = ";", description = "Default: \";\"") String separator)
			throws ConQATException {
		if (separator.length() != 1) {
			throw new ConQATException(
					"The separator must consist of exactly one character.");
		}
		char separatorChar = checkCharacterParameter(separator, "separator");
		configuration.setDelimiterChar(separatorChar);
	}

	/** Returns the parser configuration. */
	public Configuration getConfiguration() {
		return configuration;
	}

}
