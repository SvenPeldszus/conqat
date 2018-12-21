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
package org.conqat.lib.commons.xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.string.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Collection of utility methods for XML.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49617 $
 * @ConQAT.Rating GREEN Hash: D9C1451F30DEAB486EDC3BF6450C03D7
 */
public class XMLUtils {

	/**
	 * @param file  §interface§
	 *            {@link File}
	 * @return {@link Document}
	 */
	public static Document parse(File file) throws SAXException, IOException {
		return createSchemaUnawareParser().parse(file);
	}

	/**
	 * Fixes chars which are not allowed in XML content. The followoing  §interface§
	 * replacements are allowed:
	 * <ul>
	 * <li>All '&' which are not part of an XML escape char sequence are
	 * replaced by '&amp;'.
	 * <li>All low ASCII control chars are removed
	 * <li>Escaped ASCII0 char '&#0;' is removed
	 * </ul>
	 */
	public static String fixIllegalXmlChars(String content) {
		String replacedContent = content.replaceAll(
				"(?i)&(?!(lt|gt|amp|apos|quot|#x\\[0-9a-f]+|#\\d+);)", "&amp;");
		replacedContent = replacedContent.replaceAll(
				"([\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f]|&#0;)",
				StringUtils.EMPTY_STRING);
		return replacedContent;
	}
}