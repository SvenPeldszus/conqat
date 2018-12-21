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

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.test.CCSMTestCaseBase;
import org.junit.Test;

/**
 * Test class for {@link ParsedAbapElementPath}.
 * 
 * @author $Author: pfaller $
 * @version $Rev: 51045 $
 * @ConQAT.Rating GREEN Hash: 5019957C563AD6C69771CE9D80C2E2D9
 */
public class ParsedAbapElementPathTest extends CCSMTestCaseBase {

	/**
	 * Tests path parsing.
	 */
	@Test
	public void testPathParsing() throws Exception {
		ParsedAbapElementPath parsedPath;

		checkParseResult("PROG/ZTEST.abap", "PROG/ZTEST.abap",
				EAbapObjectType.PROG, "ZTEST");

		checkParseResult("PACKAGE/PROG/ZTEST.abap", "PROG/ZTEST.abap",
				EAbapObjectType.PROG, "ZTEST");

		checkParseResult("PACKAGE/PROG/ZTEST.abap", "PACKAGE", "PACKAGE",
				"PROG/ZTEST.abap", EAbapObjectType.PROG, "ZTEST");

		checkParseResult("SUPER/PACKAGE/PROG/ZTEST.abap", "SUPER/PACKAGE",
				"PACKAGE", "PROG/ZTEST.abap", EAbapObjectType.PROG, "ZTEST");

		checkParseResult("SUPER/!NAMESPACE!PACKAGE/PROG/!NAMESPACE!ZTEST.abap",
				"SUPER/!NAMESPACE!PACKAGE", "/NAMESPACE/PACKAGE",
				"PROG/!NAMESPACE!ZTEST.abap", EAbapObjectType.PROG,
				"/NAMESPACE/ZTEST");

		parsedPath = checkParseResult(
				"SUPER/!NAMESPACE!PACKAGE#lt#/PROG/!NAMESPACE!ZTEST.abap",
				"SUPER/!NAMESPACE!PACKAGE#lt#", "/NAMESPACE/PACKAGE<",
				"PROG/!NAMESPACE!ZTEST.abap", EAbapObjectType.PROG,
				"/NAMESPACE/ZTEST");
		assertEquals("Wrong super package Path", "SUPER/",
				parsedPath.getSuperPackagePath());

		checkParseResult("/module/super/package/PROG/ztest.abap",
				"/module/super/package", "PACKAGE", "PROG/ztest.abap",
				EAbapObjectType.PROG, "ZTEST");

		parsedPath = checkParseResult("/module/super/package/PROG/ztest.abap",
				"/module/super/package", "PACKAGE", "PROG/ztest.abap",
				EAbapObjectType.PROG, "ZTEST");
		assertEquals("Wrong super package path", "/module/super/",
				parsedPath.getSuperPackagePath());

		checkParseResult("SUPER/PACKAGE/CLAS/ZCL_TEST.abap", "SUPER/PACKAGE",
				"PACKAGE", "CLAS/ZCL_TEST.abap", EAbapObjectType.CLAS,
				"ZCL_TEST");

		parsedPath = checkParseResult("SUPER/PACKAGE/INTF/ZIF_TEST.abap",
				"SUPER/PACKAGE", "PACKAGE", "INTF/ZIF_TEST.abap",
				EAbapObjectType.INTF, "ZIF_TEST");
		assertEquals("Wrong class name", "ZIF_TEST", parsedPath.getClassName());

		parsedPath = checkParseResult(
				"SUPER/PACKAGE/CLAS/ZCL_TEST/ZCL_TEST======================CU.abap",
				"SUPER/PACKAGE", "PACKAGE",
				"CLAS/ZCL_TEST/ZCL_TEST======================CU.abap",
				EAbapObjectType.CLAS, "ZCL_TEST======================CU");
		assertEquals("Wrong class name", "ZCL_TEST", parsedPath.getClassName());

		parsedPath = checkParseResult(
				"CLAS/ZCL_TEST/ZCL_TEST======================CU.abap",
				"CLAS/ZCL_TEST/ZCL_TEST======================CU.abap",
				EAbapObjectType.CLAS, "ZCL_TEST======================CU");
		assertEquals("Wrong class name", "ZCL_TEST", parsedPath.getClassName());

		parsedPath = checkParseResult(
				"SUPER/PACKAGE/FUGR/ZFGROUP/LZFGROUPU01.abap", "SUPER/PACKAGE",
				"PACKAGE", "FUGR/ZFGROUP/LZFGROUPU01.abap",
				EAbapObjectType.FUGR, "LZFGROUPU01");
		assertEquals("Wrong function group", "ZFGROUP",
				parsedPath.getFunctionGroup());
	}

	/**
	 * Checks parsing results including package path
	 */
	private ParsedAbapElementPath checkParseResult(String path,
			String packagePath, String packageName, String elementPath,
			EAbapObjectType objectType, String objectName)
			throws ConQATException {
		ParsedAbapElementPath parsedPath = new ParsedAbapElementPath(path);
		assertEquals("Wrong package path.", packagePath,
				parsedPath.getPackagePath());
		assertEquals("Wrong package name.", packageName,
				parsedPath.getPackageName());
		assertEquals("Wrong element path", elementPath,
				parsedPath.getElementPath());
		assertEquals("Wrong object name.", objectName, parsedPath
				.getElementName().getObjectName());
		assertEquals("Wrong object type.", objectType, parsedPath
				.getElementName().getObjectType());
		return parsedPath;
	}

	/**
	 * Checks parsing results ignoring package path
	 */
	private ParsedAbapElementPath checkParseResult(String path,
			String elementPath, EAbapObjectType objectType, String objectName)
			throws ConQATException {
		ParsedAbapElementPath parsedPath = new ParsedAbapElementPath(path,
				false);
		assertEquals("Wrong element path", elementPath,
				parsedPath.getElementPath());
		assertEquals("Wrong object name.", objectName, parsedPath
				.getElementName().getObjectName());
		assertEquals("Wrong object type.", objectType, parsedPath
				.getElementName().getObjectType());
		return parsedPath;
	}

}
