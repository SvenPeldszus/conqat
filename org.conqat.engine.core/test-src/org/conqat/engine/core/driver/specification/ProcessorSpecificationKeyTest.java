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
package org.conqat.engine.core.driver.specification;

import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.specification.processors.ProcessorToTestSpec;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithKey;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithNonFinalKey;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithNonPublicKey;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithNonStaticKey;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithNonStringKey;

/**
 * Tests for {@link ProcessorSpecification}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: 2FE498DF22212452AB60897DC0176169
 */
public class ProcessorSpecificationKeyTest extends
		ProcessorSpecificationTestBase {

	/**
	 * Tests whether the processor {@link ProcessorWithKey} has a key specified.
	 */
	public void testKeyPresence() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				ProcessorWithKey.class.getName());

		assertEquals(1, spec.getKeys().size());
		KeySpecification key = spec.getKeys().get(0);

		assertEquals("keyName", key.getName());
		assertEquals("desc", key.getDoc());
		assertEquals("java.lang.String", key.getType());
	}

	/**
	 * Tests whether the processor {@link ProcessorToTestSpec} has no keys
	 * specified.
	 */
	public void testKeyAbsence() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				ProcessorToTestSpec.class.getName());

		assertEquals(0, spec.getKeys().size());
	}

	/** Tests expected exception of {@link ProcessorWithNonPublicKey}. */
	public void testProcessorWithNonPublicKey() {
		checkException(ProcessorWithNonPublicKey.class,
				EDriverExceptionType.KEY_NOT_PUBLIC_STATIC_FINAL);
	}

	/** Tests expected exception of {@link ProcessorWithNonStaticKey}. */
	public void testProcessorWithNonStaticKey() {
		checkException(ProcessorWithNonStaticKey.class,
				EDriverExceptionType.KEY_NOT_PUBLIC_STATIC_FINAL);
	}

	/** Tests expected exception of {@link ProcessorWithNonFinalKey}. */
	public void testProcessorWithNonFinalKey() {
		checkException(ProcessorWithNonFinalKey.class,
				EDriverExceptionType.KEY_NOT_PUBLIC_STATIC_FINAL);
	}

	/** Tests expected exception of {@link ProcessorWithNonStringKey}. */
	public void testProcessorWithNonStringKey() {
		checkException(ProcessorWithNonStringKey.class,
				EDriverExceptionType.KEY_NOT_STRING);
	}
}