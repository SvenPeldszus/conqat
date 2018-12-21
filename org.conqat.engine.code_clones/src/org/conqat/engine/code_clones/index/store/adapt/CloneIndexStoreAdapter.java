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
package org.conqat.engine.code_clones.index.store.adapt;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.conqat.engine.code_clones.index.Chunk;
import org.conqat.engine.core.driver.instance.ConQATStringPool;
import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.digest.MD5Digest;
import org.conqat.lib.commons.io.SerializationUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Clone index store based on ConQAT storage system. This implementation support
 * concurrent insertion and concurrent extraction.
 * <p>
 * We store the clone index into a single store. To differentiate between the
 * different types of keys, we prefix them with unique prefixes.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51522 $
 * @ConQAT.Rating GREEN Hash: A134BF9D00BF9D6C2F90DDCDBA2D9991
 */
public class CloneIndexStoreAdapter extends CloneIndexStoreAdapterBase {

	/** Number of bytes in an int. */
	private static final int SIZE_OF_INT = Integer.SIZE / Byte.SIZE;

	/** Prefix used for origin ID keys. */
	public static final String ORIGIN_PREFIX = "f";

	/** Prefix used for chunk hash keys. */
	private static final byte HASH_PREFIX = 'h';

	/** Size of the prefix. */
	private static final int HASH_PREFIX_SIZE = 1;

	/** Constructor. */
	public CloneIndexStoreAdapter(IStore store) {
		super(store);
	}

	/** {@inheritDoc} */
	@Override
	public void batchInsertChunks(List<Chunk> chunks) throws StorageException {
		if (chunks.isEmpty()) {
			return;
		}

		String originId = chunks.get(0).getOriginId();
		PairList<byte[], byte[]> batchData = new PairList<byte[], byte[]>();
		batchData.add(getOriginKey(originId),
				compressChunks(chunks, false, true));
		for (Chunk chunk : chunks) {
			batchData.add(createChunkKey(chunk), createChunkValue(chunk));
		}
		store.put(batchData);
	}

	/** Returns the key used for originId. */
	private static byte[] getOriginKey(String originId) {
		return StringUtils.stringToBytes((ORIGIN_PREFIX + originId));
	}

	/** {@inheritDoc} */
	@Override
	public void removeChunks(String originId) throws StorageException {
		byte[] key = getOriginKey(originId);
		byte[] value = store.get(key);
		store.remove(key);

		if (value == null) {
			return;
		}

		List<Chunk> chunks = decompressChunks(value, originId, null);
		List<byte[]> keys = new ArrayList<byte[]>();
		for (Chunk chunk : chunks) {
			keys.add(createChunkKey(chunk));
		}
		store.remove(keys);
	}

	/**
	 * Creates the key used for storing a chunk. This has to start with the hash
	 * to ensure we can efficiently access all chunks with same hash. In
	 * addition we include the origin and the starting position to ensure unique
	 * keys.
	 */
	private static byte[] createChunkKey(Chunk chunk) {
		byte[] originId = StringUtils.stringToBytes(chunk.getOriginId());
		byte[] result = new byte[HASH_PREFIX_SIZE + MD5Digest.MD5_BYTES
				+ originId.length + SIZE_OF_INT];
		result[0] = HASH_PREFIX;
		System.arraycopy(chunk.getChunkHash().getBytes(), 0, result,
				HASH_PREFIX_SIZE, MD5Digest.MD5_BYTES);
		System.arraycopy(originId, 0, result, HASH_PREFIX_SIZE
				+ MD5Digest.MD5_BYTES, originId.length);
		SerializationUtils.insertInt(chunk.getFirstUnitIndex(), result,
				result.length - SIZE_OF_INT);
		return result;
	}

	/**
	 * Creates the value used when storing a chunk. These are all parts not
	 * contained in the key from {@link #createChunkKey(Chunk)}.
	 */
	private static byte[] createChunkValue(Chunk chunk) {
		byte[] result = new byte[5 * SIZE_OF_INT];
		int offset = 0;
		SerializationUtils.insertInt(chunk.getFirstRawLineNumber(), result,
				SIZE_OF_INT * offset++);
		SerializationUtils.insertInt(chunk.getLastRawLineNumber(), result,
				SIZE_OF_INT * offset++);
		SerializationUtils.insertInt(chunk.getRawStartOffset(), result,
				SIZE_OF_INT * offset++);
		SerializationUtils.insertInt(chunk.getRawEndOffset(), result,
				SIZE_OF_INT * offset++);
		SerializationUtils.insertInt(chunk.getElementUnits(), result,
				SIZE_OF_INT * offset++);
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public List<Chunk> getChunksByOrigin(String originId)
			throws StorageException {
		byte[] value = store.get(getOriginKey(originId));
		if (value == null) {
			return null;
		}
		return decompressChunks(value, originId, null);
	}

	/** {@inheritDoc} */
	@Override
	public UnmodifiableList<Chunk> getChunksByHashes(Set<MD5Digest> chunkHashes)
			throws StorageException {
		final List<Chunk> result = new ArrayList<Chunk>();
		for (MD5Digest hash : chunkHashes) {
			byte[] prefix = new byte[HASH_PREFIX_SIZE + MD5Digest.MD5_BYTES];
			prefix[0] = HASH_PREFIX;
			System.arraycopy(hash.getBytes(), 0, prefix, HASH_PREFIX_SIZE,
					MD5Digest.MD5_BYTES);

			store.scan(prefix, new IKeyValueCallback() {
				@Override
				public void callback(byte[] key, byte[] value) {
					Chunk chunk = extractChunk(key, value);

					// the scan method may call this callback from different
					// threads.
					synchronized (result) {
						result.add(chunk);
					}
				}
			});
		}
		return CollectionUtils.asUnmodifiable(result);
	}

	/** Extracts the chunk from the given key/value pair. */
	private static Chunk extractChunk(byte[] key, byte[] value) {
		String originId = ConQATStringPool.intern(StringUtils
				.bytesToString(Arrays.copyOfRange(key, HASH_PREFIX_SIZE
						+ MD5Digest.MD5_BYTES, key.length - SIZE_OF_INT)));
		MD5Digest hash = new MD5Digest(Arrays.copyOfRange(key,
				HASH_PREFIX_SIZE, HASH_PREFIX_SIZE + MD5Digest.MD5_BYTES));
		int firstUnitIndex = SerializationUtils.extractInt(key, key.length
				- SIZE_OF_INT);

		int offset = 0;
		int firstRawLine = SerializationUtils.extractInt(value, SIZE_OF_INT
				* offset++);
		int lastRawLine = SerializationUtils.extractInt(value, SIZE_OF_INT
				* offset++);
		int rawStartOffset = SerializationUtils.extractInt(value, SIZE_OF_INT
				* offset++);
		int rawEndOffset = SerializationUtils.extractInt(value, SIZE_OF_INT
				* offset++);
		int unitCount = SerializationUtils.extractInt(value, SIZE_OF_INT
				* offset++);
		return new Chunk(originId, hash, firstUnitIndex, firstRawLine,
				lastRawLine, rawStartOffset, rawEndOffset, unitCount);
	}
}
