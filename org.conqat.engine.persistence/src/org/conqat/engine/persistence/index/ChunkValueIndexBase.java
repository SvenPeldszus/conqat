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
package org.conqat.engine.persistence.index;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.io.ByteArrayUtils;
import org.conqat.lib.commons.io.ChunkInputStream;
import org.conqat.lib.commons.io.ChunkOutputStream;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Abstract base class for indexes storing single values for string keys. If
 * values are too big, they will be split into multiple chunks.
 * 
 * Chunks will be stored using the string key and appending the chunk's index
 * (counting from one) after it. The index is represented as byte array of
 * length four. The number of chunks is stored in the first key with index zero.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50465 $
 * @ConQAT.Rating GREEN Hash: 87CEEEF43AC62B309366354AB4F7AD3E
 */
public abstract class ChunkValueIndexBase<T> extends IndexBase {

	/** Number of chunks value, if number of chunks could not be loaded. */
	private static final int CHUNK_NOT_EXISTING = -1;

	/** The internal chunk size. */
	private int chunkSize;

	/** Constructor. Initialize chunk size with pow(2,20). */
	public ChunkValueIndexBase(IStore store) {
		this(store, 1 << 20);
	}

	/** Constructor, which sets the desired chunk size. */
	public ChunkValueIndexBase(IStore store, int chunkSize) {
		super(store);
		this.chunkSize = chunkSize;
	}

	/** Returns the chunk size. */
	public int getChunkSize() {
		return chunkSize;
	}

	/**
	 * Loads the value for the given key.
	 * 
	 * @throws StorageException
	 *             thrown if loading the value from the backend store failed
	 */
	public T getValue(String key) throws StorageException {
		return getValueFromChunks(getChunks(key));
	}

	/** Converts the given chunk into a value. */
	private T getValueFromChunks(List<byte[]> chunks) throws StorageException {
		if (chunks == null) {
			return null;
		}

		try (ChunkInputStream in = new ChunkInputStream(chunks)) {
			return readValueFromStream(in);
		}
	}

	/** Returns all chunks for the given key. */
	private List<byte[]> getChunks(String key) throws StorageException {
		int numberOfChunks = getNumberOfChunks(key);
		if (numberOfChunks == CHUNK_NOT_EXISTING) {
			return null;
		}

		List<byte[]> chunks = new ArrayList<>(numberOfChunks);
		for (int i = 0; i < numberOfChunks; i++) {
			byte[] chunk = getChunk(key, i + 1);
			if (chunk == null) {
				return null;
			}
			chunks.add(chunk);
		}

		return chunks;
	}

	/**
	 * Read the number of chunks for the given key. Returns -1 if no entry for
	 * the given key exists.
	 */
	private int getNumberOfChunks(String key) throws StorageException {
		byte[] chunk = getChunk(key, 0);
		if (chunk == null) {
			return CHUNK_NOT_EXISTING;
		}
		if (chunk.length != ByteArrayUtils.INT_BYTE_ARRAY_LENGTH) {
			throw new StorageException("Expected "
					+ ByteArrayUtils.INT_BYTE_ARRAY_LENGTH
					+ " bytes as number of chunks!");
		}

		return ByteArrayUtils.byteArrayToInt(chunk);
	}

	/** Loads the chunk with the given key and index from the backend store. */
	private byte[] getChunk(String key, int chunkIndex) throws StorageException {
		byte[] keyBytes = getKeyBytes(key, chunkIndex);
		return store.get(keyBytes);
	}

	/**
	 * Returns the key bytes for the given string key with the given chunk
	 * index. The integer's four bytes are appended to the string bytes.
	 */
	private byte[] getKeyBytes(String key, int chunkIndex) {
		return ByteArrayUtils.concat(StringUtils.stringToBytes(key),
				ByteArrayUtils.intToByteArray(chunkIndex));
	}

	/**
	 * Sets the value for the given key.
	 * 
	 * @throws StorageException
	 *             thrown if storing the value to the backend store failed
	 */
	public void setValue(String key, T value) throws StorageException {
		List<byte[]> chunks = getChunksFromValue(value);
		int numberOfChunks = chunks.size();

		setChunk(key, ByteArrayUtils.intToByteArray(numberOfChunks), 0);
		setChunks(key, chunks);
	}

	/** Converts the given value to chunks. */
	private List<byte[]> getChunksFromValue(T value) throws StorageException {
		try (ChunkOutputStream out = new ChunkOutputStream(this.chunkSize)) {
			writeValueToStream(out, value);
			List<byte[]> chunks = out.getChunks();

			if (!chunks.isEmpty()) {
				int lastChunkSize = out.getLastChunkSize();
				int lastChunkIndex = chunks.size() - 1;

				// if the last chunk's size is smaller than the chunk size,
				// shrink it
				if (lastChunkSize < chunkSize) {
					chunks.set(lastChunkIndex, Arrays.copyOf(
							chunks.get(lastChunkIndex), lastChunkSize));
				}
			}
			return chunks;
		}
	}

	/** Writes the given chunks for the given key to the backend store. */
	private void setChunks(String key, List<byte[]> chunks)
			throws StorageException {
		for (int i = 0; i < chunks.size(); i++) {
			setChunk(key, chunks.get(i), i + 1);
		}
	}

	/** Writes the given chunk to the backend store. */
	private void setChunk(String key, byte[] chunk, int chunkIndex)
			throws StorageException {
		store.put(getKeyBytes(key, chunkIndex), chunk);
	}

	/** Reads a value from the given InputStream. */
	protected abstract T readValueFromStream(InputStream in)
			throws StorageException;

	/** Writes the given value to the given OutputStream. */
	protected abstract void writeValueToStream(OutputStream out, T value)
			throws StorageException;
}
