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
package org.conqat.engine.abap;

import java.nio.charset.Charset;
import java.util.TimeZone;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.assertion.CCSMAssert;

/**
 * Core utility functionality for ABAP analysis.
 *
 * @author $Author: pfaller $
 * @version $Rev: 51274 $
 * @ConQAT.Rating YELLOW Hash: 71C20B428FF627F653A33A9D94FC5681
 */
public class AbapCoreUtils {

	/** Ant pattern matching ABAP source files */
	public static final String ABAP_SOURCE_FILES_PATTERN = "**.abap";

	/**
	 * Encoding for content of ABAP export Zip files In the ABAP exporter, the
	 * encoding is defined in ZCL_CQSE_ABAP_UTILS=>GC_ABAP_ENCODING with value
	 * '1100', what refers to 'iso-8859-1' (see
	 * http://wiki.scn.sap.com/wiki/display/ABAP/Character+encoding+conversion).
	 */
	public static final Charset ABAP_ENCODING = Charset.forName("iso-8859-1");

	/**
	 * Creates a ABAP element name from the given path. The path must point to
	 * an ABAP element (see Patterns in {@link ParsedAbapElementPath}).
	 */
	public static UniqueAbapElementName createElementNameFromPath(String path) {
		try {
			ParsedAbapElementPath parsedPath = new ParsedAbapElementPath(path,
					false);
			return parsedPath.getElementName();
		} catch (ConQATException e) {
			CCSMAssert
					.fail("Path is not expected to point to an ABAP element: "
							+ path);
			return null;
		}
	}

	/**
	 * Formats an ABAP identifier to the standard representation used in ConQAT.
	 * That is '/' are replaced by '!' and the identifier is put to uppercase.
	 **/
	public static String formatAbapIdentifier(String functionName) {
		return functionName.replace('/', '!').toUpperCase();
	}

	/**
	 * Transforms file names (and parts of paths) to the ABAP name. This method
	 * reverts the character replacements which were necessary during ABAP
	 * export to build valid file names. This method should return the original
	 * ABAP name.
	 */
	public static String convertFileNameToAbapName(String s) {

		// When changing this method also change ABAP method
		// ZCL_CQSE_ABAP_UTIL->tidy_file_name
		// accordingly

		return s.replace("!", "/").replace("#excl#", "!")
				.replace("#rsol#", "\\").replace("#lt#", "<")
				.replace("#gt#", ">").replace("#colon#", ":")
				.replace("#verbar#", "|").replace("#quest#", "?")
				.replace("#ast#", "*").replace("#quot#", "\"").toUpperCase();
	}

}
