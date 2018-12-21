/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.abap
 |                                                                       |
   $Id: CSVExternalRecordAnnotatorBase.java 51578 2015-01-21 07:54:09Z goeb $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.io.external_records;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;

import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.io.csv.CSVEnumParser;
import org.conqat.engine.io.csv.CSVParserConfiguration;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.base.ElementTraversingProcessorBase;
import org.conqat.engine.resource.text.ITextElement;

/**
 * Base class for processors which read data from CSV files and store the date
 * at corresponding {@link IElement}s.
 * 
 * The first line of the CSV file is treated as header which holds the column
 * names. The column names must be the same is in the enumeration type <C>.
 * 
 * For parsing, <a href="http://supercsv.sourceforge.net/">Simple CSV</a> is
 * used.
 * 
 * Each further line in the CSV file must correspond to a an Element of the
 * input node. Multiple lines may be annotated to the same Element. To determine
 * if a line and an element correspond to each other
 * {@link #getRecordElementIdentifier(ExternalDataRecord)} and
 * {@link #getElementIdentifier(IElement)} are used.
 * 
 * The resulting lines are stored under the key given by {@link #getKey()} at
 * the elements.
 * 
 * @param <R>
 *            the type of resources on the input scope to be traversed (holds
 *            the elements which should receive the annotation)
 * 
 * @param <E>
 *            the type of element this works on. The element class should
 *            implement R and must match with the class returned from
 *            {@link ElementTraversingProcessorBase#getElementClass()}
 * @param <C>
 *            enumeration type of column names. The column names are supposed to
 *            conform to the format specified in
 *            {@link CSVEnumParser#normalizeHeader}
 * 
 * @author $Author: goeb $
 * @version $Rev: 51578 $
 * @ConQAT.Rating GREEN Hash: E79FF909974F365F2D97892D1FB2AD1A
 */
public abstract class CSVExternalRecordAnnotatorBase<R extends IResource, E extends IElement, C extends Enum<C>>
		extends ExternalDataAnnotatorBase<R, E, C> {

	/** Holds the parser configuration. */
	@AConQATParameterObject
	public CSVParserConfiguration parserConfiguration = new CSVParserConfiguration();

	/** Constructor. */
	protected CSVExternalRecordAnnotatorBase(Class<C> columnsEnumClass) {
		super(columnsEnumClass);
	}

	/**
	 * Parses a an {@link ITextElement} as CSV file and returns a list of parsed
	 * lines.
	 */
	@Override
	protected List<ExternalDataRecord<C>> parseExternalDataElement(
			ITextElement element) throws ConQATException {
		CSVEnumParser<C> parser = new CSVEnumParser<>(fieldsEnumClass,
				parserConfiguration.getConfiguration());
		parser.parseAndWrapExceptions(new StringReader(element.getTextContent()));

		List<ExternalDataRecord<C>> parsedLines = new ArrayList<ExternalDataRecord<C>>();
		for (EnumMap<C, String> row : parser) {
			ExternalDataRecord<C> parsedCsvLine = new ExternalDataRecord<C>(row);
			parsedLines.add(parsedCsvLine);
		}

		return parsedLines;
	}

}
