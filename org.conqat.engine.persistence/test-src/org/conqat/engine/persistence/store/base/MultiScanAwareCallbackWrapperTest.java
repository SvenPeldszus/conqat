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
package org.conqat.engine.persistence.store.base;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.base.PartitionStoreBase.MultiScanAwareCallbackWrapper;
import org.conqat.engine.persistence.store.util.KeyCollectingCallback;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Tests the {@link MultiScanAwareCallbackWrapper} .
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47973 $
 * @ConQAT.Rating GREEN Hash: 167250A387FB4A301060063DB37DED90
 */
public class MultiScanAwareCallbackWrapperTest extends CCSMTestCaseBase {

	/** Tests scanning with all different keys. */
	public void testScanAllDifferent() {
		List<byte[]> keys = new ArrayList<>();
		IKeyValueCallback callback = new PartitionStoreBase.MultiScanAwareCallbackWrapper(
				new KeyCollectingCallback(keys));
		callback.callback(new byte[] { 40, 50, 60 }, null);
		callback.callback(new byte[] { 40, 51, 60 }, null);
		callback.callback(new byte[] { 40, 52, 60 }, null);
		assertEquals(3, keys.size());
	}

	/** Tests scanning with some identical keys. */
	public void testScanIdenticalKeys() {
		List<byte[]> keys = new ArrayList<>();
		IKeyValueCallback callback = new PartitionStoreBase.MultiScanAwareCallbackWrapper(
				new KeyCollectingCallback(keys));
		callback.callback(new byte[] { 40, 50, 60 }, null);
		callback.callback(new byte[] { 40, 51, 60 }, null);
		callback.callback(new byte[] { 40, 50, 60 }, null);
		assertEquals(2, keys.size());
	}

	/** Tests the bug from CR#6030. */
	public void testScanCR6030() {
		List<byte[]> keys = new ArrayList<>();
		IKeyValueCallback callback = new PartitionStoreBase.MultiScanAwareCallbackWrapper(
				new KeyCollectingCallback(keys));
		callback.callback(new byte[] { 0, 0, 0, -3, 50, 60 }, null);
		callback.callback(new byte[] { 0, 0, 0, -3, 51, 60 }, null);
		assertEquals(2, keys.size());
	}

}
