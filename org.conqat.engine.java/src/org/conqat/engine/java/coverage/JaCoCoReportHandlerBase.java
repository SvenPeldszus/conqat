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
package org.conqat.engine.java.coverage;

import java.io.IOException;

import org.conqat.engine.java.BundleContext;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Base class for SAX handlers that read JaCoCo XML reports.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51571 $
 * @ConQAT.Rating GREEN Hash: F8623824FF59894AAD5D3300C6577BD9
 */
public class JaCoCoReportHandlerBase extends DefaultHandler {

	/** The file name of the Jacoco report DTD */
	public static final String JACOCO_REPORT_DTD_FILE = "jacoco-report.dtd";

	/** The public ID of the Jacoco report DTD file */
	public static final String JACOCO_DTD_ID = "-//JACOCO//DTD Report 1.0//EN";

	/** The XML element {@value} . */
	public static final String LINE_ELEMENT = "line";

	/** The XML element {@value} . */
	public static final String SOURCEFILE_ELEMENT = "sourcefile";

	/** The XML element {@value} . */
	public static final String METHOD_ELEMENT = "method";

	/** The XML element {@value} . */
	public static final String CLASS_ELEMENT = "class";

	/** The XML element {@value} . */
	public static final String PACKAGE_ELEMENT = "package";

	/** The XML element {@value} . */
	public static final String COUNTER_ELEMENT = "counter";

	/**
	 * The XML attribute for the name of packages and source files ({@value} ).
	 */
	public static final String NAME_ATTRIBUTE = "name";

	/** The XML attribute for the type signature ({@value} ) . */
	public static final String DESC_ATTRIBUTE = "desc";

	/** The XML attribute for the counter type ({@value} ) . */
	public static final String TYPE_ATTRIBUTE = "type";

	/** The XML attribute for a counter's "covered" count ({@value} ) . */
	public static final String COVERED_ATTRIBUTE = "covered";

	/** The counter type for method coverage ({@value} ) . */
	public static final String COUNTER_TYPE_METHOD = "METHOD";

	/** The XML attribute for the line number ({@value} ) . */
	public static final String LINE_NUMBER_ATTRIBUTE = "nr";

	/** The XML attribute for the number of covered instructions ({@value} ) . */
	public static final String COVERED_INSTRUCTIONS_ATTRIBUTE = "ci";

	/** The XML attribute for the number of missed instructions ({@value} ) . */
	public static final String MISSED_INSTRUCTIONS_ATTRIBUTE = "mi";

	/** The XML attribute for the number of missed branches ({@value} ) . */
	public static final String MISSED_BRANCHES_ATTRIBUTE = "mb";

	/** {@inheritDoc} */
	@Override
	public InputSource resolveEntity(String publicId, String systemId)
			throws IOException {
		if (publicId.equals(JACOCO_DTD_ID)) {
			return new InputSource(BundleContext.getInstance()
					.getResourceManager()
					.getResourceAsStream(JACOCO_REPORT_DTD_FILE));
		}
		return null;
	}

}
