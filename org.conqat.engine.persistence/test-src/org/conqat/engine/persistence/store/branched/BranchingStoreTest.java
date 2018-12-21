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
package org.conqat.engine.persistence.store.branched;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.conqat.engine.persistence.index.IndexBase;
import org.conqat.engine.persistence.index.schema.EStorageOption;
import org.conqat.engine.persistence.index.schema.SchemaEntry;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.StorageSystemTestBase.CollectingCallBack;
import org.conqat.engine.persistence.store.hist.HeadReadOnlyHistorizingStore;
import org.conqat.engine.persistence.store.hist.HistoryAccessOption;
import org.conqat.engine.persistence.store.mem.InMemoryStore;
import org.conqat.engine.persistence.store.util.ConvenientStore;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.io.ByteArrayUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Tests the {@link BranchCommitReadingStore} and the
 * {@link BranchCommitInsertingStore}.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51715 $
 * @ConQAT.Rating GREEN Hash: 0AFFFD59C1253B28A4F918A097FDAABB
 */
public class BranchingStoreTest extends CCSMTestCaseBase {

	/** The base store used for writing. */
	private IStore baseStore;

	/** {@inheritDoc} */
	@Override
	protected void setUp() throws Exception {
		super.setUp();

		baseStore = new InMemoryStore();

		// insert some data at rev 12
		ConvenientStore rev12 = new ConvenientStore(
				new BranchCommitInsertingStore(baseStore, commitName(12), null));
		PairList<String, byte[]> keysValues = new PairList<String, byte[]>();
		keysValues.add("key1", new byte[] { 1 });
		keysValues.add("key2", new byte[] { 2 });
		keysValues.add("key3", new byte[] { 3 });
		rev12.putWithStrings(keysValues);

		// some changes at rev 27
		ConvenientStore rev27 = new ConvenientStore(
				new BranchCommitInsertingStore(baseStore, commitName(27),
						commitName(12)));
		rev27.removeWithString("key2");
		rev27.putWithString("key4", new byte[] { 4 });
		rev27.putWithString("key1", new byte[] { 5 });

		// more changes at rev 42
		ConvenientStore rev42 = new ConvenientStore(
				new BranchCommitInsertingStore(baseStore, commitName(42),
						commitName(27)));
		rev42.removeWithString("key1");
		rev42.putWithString("key2", new byte[] { 6 });
		rev42.putWithString("key4", new byte[] { 7 });
	}

	/** Creates a symbolic name for the commit with given id. */
	private byte[] commitName(long timestamp) {
		return ByteArrayUtils.longToByteArray(timestamp);
	}

	/** Tests reading from the head. */
	public void testHeadReadingStore() throws StorageException {
		ConvenientStore rev42 = new ConvenientStore(
				new BranchCommitReadingStore(baseStore, commitName(42)));

		assertNull(rev42.getWithString("key1"));
		assertEquals(6, rev42.getWithString("key2")[0]);
		assertEquals(3, rev42.getWithString("key3")[0]);
		assertEquals(7, rev42.getWithString("key4")[0]);

		CollectingCallBack callback = new CollectingCallBack();
		rev42.scan("", callback);
		assertKeys(callback.keys, "key2", "key3", "key4");

		callback.keys.clear();
		rev42.scan(StringUtils.stringToBytes("key1"),
				StringUtils.stringToBytes("key4"), callback);
		assertKeys(callback.keys, "key2", "key3");
	}

	/** Tests reading using a fixed revision. */
	public void testFixedRevisionReadingStore() throws StorageException {
		ConvenientStore rev27 = new ConvenientStore(
				new BranchCommitReadingStore(baseStore, commitName(27)));

		assertEquals(5, rev27.getWithString("key1")[0]);
		assertNull(rev27.getWithString("key2"));
		assertEquals(3, rev27.getWithString("key3")[0]);
		assertEquals(4, rev27.getWithString("key4")[0]);

		CollectingCallBack callback = new CollectingCallBack();
		rev27.scan("", callback);
		assertKeys(callback.keys, "key1", "key3", "key4");

		callback.keys.clear();
		rev27.scan(StringUtils.stringToBytes("key1"),
				StringUtils.stringToBytes("key4"), callback);
		assertKeys(callback.keys, "key1", "key3");
	}

	/** Test that checks if data deduplication works as expected. */
	public void testDataDeduplication() throws StorageException {
		StorageUtils.clearStore(baseStore);
		byte[] muchData = StringUtils.stringToBytes(StringUtils
				.randomString(1000));
		ConvenientStore store = new ConvenientStore(
				new BranchCommitInsertingStore(baseStore, new byte[] { 1 },
						null));

		String[] keys = new String[] { "a", "b", "c", "d" };

		for (String key : keys) {
			store.putWithString(key, muchData);
		}

		for (String key : keys) {
			assertTrue(Arrays.equals(muchData, store.getWithString(key)));
		}

		assertEquals(muchData.length * keys.length, getValueSize(store));

		// we know we will have more than muchData.length, but not too much more
		// due to deduplication
		assertTrue(getValueSize(baseStore) < 2 * muchData.length);
	}

	/** Returns the size in bytes of all values stored in the store. */
	private static int getValueSize(IStore store) throws StorageException {
		int valueSize = 0;
		for (byte[] key : StorageUtils.listKeys(store)) {
			valueSize += store.get(key).length;
		}
		return valueSize;
	}

	/** Assert the keys match. */
	private static void assertKeys(List<String> actual, String... expected) {
		assertEquals(new HashSet<String>(Arrays.asList(expected)),
				new HashSet<String>(actual));
	}

	/**
	 * Tests rollback to a very early timestamp, which should remove all
	 * content.
	 */
	public void testRollbackToStart() throws StorageException {
		BranchedStoreUtils.deleteCommitsAfter(baseStore, 1);
		assertEquals(0, StorageUtils.keyCount(baseStore));
	}

	/**
	 * Tests rollback to a very late timestamp, which should have no effect at
	 * all.
	 */
	public void testRollbackToLateTimestamp() throws StorageException {
		String previous = StorageUtils.serializeStoreForTest(baseStore);
		BranchedStoreUtils.deleteCommitsAfter(baseStore, 1000);
		assertEquals(previous, StorageUtils.serializeStoreForTest(baseStore));
	}

	/** Tests rollback to a specific timestamp. */
	public void testRollbackToSpecificTimestamp() throws StorageException {
		BranchedStoreUtils.deleteCommitsAfter(baseStore, 20);

		IStore headView = HistoryAccessOption.readHead().createStore(baseStore,
				new SchemaEntry(IndexBase.class, EStorageOption.BRANCHED));
		assertEquals(3, StorageUtils.keyCount(headView));
	}
}
