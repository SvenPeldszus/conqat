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
import org.conqat.lib.commons.xml.OfflineSAXHandlerBase;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Base class for SAX handlers that read Cobertura XML reports.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51652 $
 * @ConQAT.Rating GREEN Hash: EDD12BBD5A128CA9E4ED1025B9B5D0D5
 */
public class CoberturaReportHandlerBase extends OfflineSAXHandlerBase {

	/** The file name of the Cobertura report DTD */
	public static final String COBERTURA_REPORT_DTD_FILE = "cobertura-report.dtd";

	/** The public ID of the Cobertura report DTD file */
	public static final String COBERTURA_DTD_ID = "http://cobertura.sourceforge.net/xml/coverage-04.dtd";

	/** The XML element {@value} . */
	public static final String COVERAGE_ELEMENT = "coverage";

	/** The XML element {@value} . */
	public static final String CLASS_ELEMENT = "class";

	/** The XML element {@value} . */
	public static final String METHODS_ELEMENT = "methods";

	/** The XML element {@value} . */
	public static final String LINE_ELEMENT = "line";

	/**
	 * The XML attribute for the trace time ({@value} ).
	 */
	public static final String TIMESTAMP_ATTRIBUTE = "timestamp";

	/**
	 * The XML attribute for the name of source files ({@value} ).
	 */
	public static final String FILENAME_ATTRIBUTE = "filename";

	/**
	 * The XML attribute for the name of classes ({@value} ).
	 */
	public static final String NAME_ATTRIBUTE = "name";

	/**
	 * The XML attribute for the line number ({@value} ).
	 */
	public static final String LINE_NUMBER_ATTRIBUTE = "number";

	/**
	 * The XML attribute for the hits ({@value} ).
	 */
	public static final String HITS_ATTRIBUTE = "hits";

	/**
	 * The XML attribute for branch lines ({@value} ).
	 */
	public static final String BRANCH_ATTRIBUTE = "branch";

	/**
	 * The XML attribute for the condition coverage ({@value} ).
	 */
	public static final String CONDITION_COVERAGE_ATTRIBUTE = "condition-coverage";

	/** {@inheritDoc} */
	@Override
	public InputSource resolveEntity(String publicId, String systemId)
			throws IOException, SAXException {
		if (systemId.equals(COBERTURA_DTD_ID)) {
			return new InputSource(BundleContext.getInstance()
					.getResourceManager()
					.getResourceAsStream(COBERTURA_REPORT_DTD_FILE));
		}
		return super.resolveEntity(publicId, systemId);
	}
}
