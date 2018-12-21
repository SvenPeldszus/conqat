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
package org.conqat.engine.dotnet.test.mstest;

/**
 * Enumeration of XML attributes required to read .trx-files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 289BC29D150B5332400DDAB46AFF7AED
 */
/* package */enum ETrxAttribute {
	/** The name of the test run or test method. */
	name,

	/** The start time of the test run. */
	start,

	/** The end time of of the test run. */
	endTime,

	/** The id of the test (TestMethod element context). */
	testId,

	/** The id of the test (UnitTest element context). */
	id,

	/** The duration of a single test method. */
	duration,

	/** The outcome of the test run */
	outcome,

	/** The working directory of a test run. */
	runDeploymentRoot,

	/** The URI of the test run data collector. */
	uri,

	/** The (full classified) name of a test class. */
	className,

	/** The name of the test assembly corresponding to a test method. */
	codeBase;
}
