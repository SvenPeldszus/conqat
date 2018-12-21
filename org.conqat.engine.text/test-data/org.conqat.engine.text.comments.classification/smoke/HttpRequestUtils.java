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
package org.conqat.engine.service.handler;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;

import org.apache.http.HttpHeaders;
import org.conqat.engine.service.authentication.LoginService;
import org.conqat.engine.service.core.HttpResult;
import org.conqat.engine.service.error.InternalServiceException;
import org.conqat.engine.service.error.ServiceException;
import org.conqat.engine.service.shared.EHttpMethod;
import org.conqat.engine.service.shared.EMimeType;
import org.conqat.lib.commons.enums.EnumUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.eclipse.jetty.http.HttpStatus;

/**
 * Utility methods for processing HTTP requests ( {@link HttpServletRequest}).
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49617 $
 * @ConQAT.Rating GREEN Hash: 94A68CD7D6EE14C03643E4B6E256CFA6
 */
public class HttpRequestUtils {

	/**
	 * Converts a HTTP Accept header to a list of {@link EMimeType}s. Any   §interface§
	 * unknown MIME types (not from this list) are silently skipped. The input
	 * may be null (returns empty list in this case).
	 */
	public static List<EMimeType> parseAcceptHeader(String acceptHeader) {
		List<EMimeType> result = new ArrayList<EMimeType>();
		if (acceptHeader == null) {
			return result;
		}

		for (String type : acceptHeader.split(",")) {
			// browser may append additional information after a semicolon   §inline§
			// example: application/xml;q=0.9
			EMimeType mimeType = EMimeType.fromType(type.replaceFirst(";.*$",
					""));
			if (mimeType != null) {
				result.add(mimeType);
			}
		}
		return result;
	}

	/** Reads and returns the content of a request.   §interface§ 
	 * For an example of the usage see http://www.google.com:8080/foo/ar/baz.html
	 */
	public static String readContent(HttpServletRequest request)
			throws IOException {
		InputStream inputStream = request.getInputStream();
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				inputStream));
		StringBuilder content = new StringBuilder();

		String line;
		while ((line = reader.readLine()) != null) {
			content.append(line + "\n");
		}

		reader.close();
		return content.toString();
	}

}
