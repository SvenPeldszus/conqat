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
package org.conqat.engine.commons.statistics;

import java.util.LinkedHashMap;
import java.util.Map;

import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Tests the {@link KeyedDataTopFilter}.
 * 
 * @author $Author: pfaller $
 * @version $Rev: 48799 $
 * @ConQAT.Rating GREEN Hash: DBFADB5337235A1410D14F85A8767C16
 */
public class KeyedDataTopFilterTest extends CCSMTestCaseBase {

	/** Tests truncation and summation of maps. */
	public void testMapTruncation() {
		Map<String, Double> values = new LinkedHashMap<>();
		values.put("one", 1.0);
		values.put("two", 1.0);
		values.put("three", 1.0);
		values.put("four", 1.0);
		values.put("five", 1.0);

		Map<String, Double> tempMap = new LinkedHashMap<>(values);
		assertEquals(0.0, KeyedDataTopFilter.removeAndSumUpAfter(tempMap, 5));
		assertEquals(5, tempMap.size());
		assertEquals(1.0, KeyedDataTopFilter.removeAndSumUpAfter(tempMap, 4));
		assertEquals(4, tempMap.size());

		tempMap = new LinkedHashMap<>(values);
		assertEquals(5.0, KeyedDataTopFilter.removeAndSumUpAfter(tempMap, 0));
		assertEquals(0, tempMap.size());

		tempMap = new LinkedHashMap<>(values);
		assertEquals(4.0, KeyedDataTopFilter.removeAndSumUpAfter(tempMap, 1));
		assertEquals(1, tempMap.size());

		tempMap = new LinkedHashMap<>(values);
		assertEquals(5.0, KeyedDataTopFilter.removeAndSumUpAfter(tempMap, -10));
		assertEquals(0, tempMap.size());
	}
}
