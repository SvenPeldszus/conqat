/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.testgap
 |                                                                       |
   $Id: DateHTMLFormatter.java 13780 2014-07-22 07:47:53Z goeb $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.html_presentation.formatters;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.conqat.lib.commons.html.HTMLWriter;

/**
 * An HTML formatter for dates based on {@link SimpleDateFormat} patterns.
 * 
 * @author $Author: goeb $
 * @version $Rev: 13780 $
 * @ConQAT.Rating GREEN Hash: 49643D6DB3D198DAD50A3D39985CD6D4
 */
public class DateHTMLFormatter implements IHTMLFormatter<Date> {

	/** Default pattern for date serialization. */
	private static final String DEFAULT_PATTERN = "yyyy-MM-dd HH:mm:ss";

	/** Date format to serialize dates to strings. */
	private final SimpleDateFormat format;

	/** Constructor using provided {@link SimpleDateFormat} pattern. */
	public DateHTMLFormatter(String pattern) {
		this.format = new SimpleDateFormat(pattern);
	}

	/** Constructor using {@value #DEFAULT_PATTERN}. */
	public DateHTMLFormatter() {
		this(DEFAULT_PATTERN);
	}

	/**
	 * Adds the date as text using the provided date format.
	 */
	@Override
	public void formatObject(Date date, HTMLWriter writer) {
		writer.addText(format.format(date));
	}
}