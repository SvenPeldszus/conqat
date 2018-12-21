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
package org.conqat.engine.text.comments.classification;

import org.conqat.lib.scanner.ELanguage;

import junit.framework.TestCase;

/**
 * Tests for the {@link CoherenceUtils}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49710 $
 * @ConQAT.Rating GREEN Hash: A8BA65FC16CF9F78D22046099F7F2172
 */
public class CoherenceUtilsTest extends TestCase {

	/** Test for a field involving a stemming scenario. See also CR#6018. */
	public void testField() throws Exception {
		String fieldName = "validFrom";
		String comment = "/** The first day of validity */";
		assertEquals(1, CoherenceUtils.getNumCorrespondingWords(fieldName,
				comment, ELanguage.JAVA));
	}

}
