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
package org.conqat.engine.dotnet.test.xunit;

/**
 * Enumeration of XML attributes required to read XUnit XML files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 7243E62A3224E14A24C82E914C2F43D8
 */
/* package */enum EXUnitAttribute {

	/** The name of the test. */
	NAME,

	/** The time of the test run. */
	TIME,

	/** The test outcome. */
	OUTCOME,

	/** The test result. */
	RESULT,

	/** The test-method name. */
	METHOD,

	/** Test type of the test (class). */
	TYPE,

	/** The time the test was run. */
	RUN_TIME,

	/** The date the test was run. */
	RUN_DATE;
}