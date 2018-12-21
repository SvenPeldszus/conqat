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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.code_clones.index.Chunk;
import org.conqat.engine.code_clones.index.store.ICloneIndexStore;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.digest.MD5Digest;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.io.SerializationUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Base class for a Clone index store based on the ConQAT storage system. This
 * provides utility methods and deals with option handling.
 * <p>
 * We store the clone index into a single store. To differentiate between the
 * different types of keys, we prefix them with unique prefixes.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51522 $
 * @ConQAT.Rating GREEN Hash: C4613B34B7706726668E0F6A7384DBC0
 */
public abstract class CloneIndexStoreAdapterBase implements ICloneIndexStore {

	/** Prefix used for storing options. */
	private static final String OPTION_PREFIX = "o";

	/** The store used. */
	protected final IStore store;

	/** Constructor. */
	protected CloneIndexStoreAdapterBase(IStore store) {
		this.store = store;
	}

	/** {@inheritDoc} */
	@Override
	public Serializable getOption(String key) throws StorageException {
		try {
			byte[] value = store.get(getOptionsKey(key));
			if (value == null) {
				return null;
			}

			// we have to use the thread's context class loader, as in the
			// ConQAT world this is where the classes are loaded from
			return SerializationUtils.deserializeFromByteArray(value, Thread
					.currentThread().getContextClassLoader());
		} catch (IOException e) {
			throw new StorageException("Could not deserialize option: " + key,
					e);
		} catch (ClassNotFoundException e) {
			throw new StorageException("Could not create option: " + key, e);
		}
	}

	/** Returns the key used for storing options. */
	private static byte[] getOptionsKey(String key) {
		return StringUtils.stringToBytes((OPTION_PREFIX + key));
	}

	/** {@inheritDoc} */
	@Override
	public void setOption(String key, Serializable value)
			throws StorageException {
		try {
			store.put(getOptionsKey(key),
					SerializationUtils.serializeToByteArray(value));
		} catch (IOException e) {
			throw new StorageException("Could not set option: " + key, e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void close() {
		// nothing to do
	}

	/**
	 * Calculates a compressed representation for the given list of chunks which
	 * all belong to the same origin. The originId is not stored in the list!
	 */
	protected static byte[] compressChunks(List<Chunk> chunks,
			boolean includeOrigin, boolean includeHash) throws StorageException {
		try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
				DataOutputStream dos = new DataOutputStream(bos)) {
			dos.writeInt(chunks.size());
			for (Chunk chunk : chunks) {
				writeChunk(chunk, dos, includeOrigin, includeHash);
			}
			return bos.toByteArray();
		} catch (IOException e) {
			throw new StorageException(
					"Should not be possible as we are writing to memory!", e);
		}
	}

	/** Writes the given chunk (without name) into the stream. */
	private static void writeChunk(Chunk chunk, DataOutputStream dos,
			boolean includeOrigin, boolean includeHash) throws IOException {
		if (includeOrigin) {
			dos.writeUTF(chunk.getOriginId());
		}
		if (includeHash) {
			dos.write(chunk.getChunkHash().getBytes());
		}
		dos.writeInt(chunk.getFirstUnitIndex());
		dos.writeInt(chunk.getFirstRawLineNumber());
		dos.writeInt(chunk.getLastRawLineNumber());
		dos.writeInt(chunk.getRawStartOffset());
		dos.writeInt(chunk.getRawEndOffset());
		dos.writeInt(chunk.getElementUnits());
	}

	/**
	 * Decompresses a list of chunks compressed with
	 * {@link #compressChunks(List, boolean, boolean)}.
	 *
	 * @param originId
	 *            if this is non-null, the origin ID is not read from the data,
	 *            but rather this value is used.
	 * @param chunkHash
	 *            if this is non-null, the chunk's hash is not not read from the
	 *            data, but rather this value is used.
	 */
	protected static List<Chunk> decompressChunks(byte[] data, String originId,
			MD5Digest chunkHash) throws StorageException {
		try (DataInputStream dis = new DataInputStream(
				new ByteArrayInputStream(data))) {
			int size = dis.readInt();
			List<Chunk> result = new ArrayList<Chunk>(size);
			for (int i = 0; i < size; ++i) {
				result.add(readChunk(dis, originId, chunkHash));
			}
			return result;
		} catch (IOException e) {
			throw new StorageException(
					"Should not happen, as operates in memory!", e);
		}
	}

	/** Reads a chunk . */
	private static Chunk readChunk(DataInputStream dis, String originId,
			MD5Digest chunkHash) throws IOException {

		if (originId == null) {
			originId = dis.readUTF();
		}

		if (chunkHash == null) {
			byte[] bytes = new byte[MD5Digest.MD5_BYTES];
			FileSystemUtils.safeRead(dis, bytes);
			chunkHash = new MD5Digest(bytes);
		}

		return new Chunk(originId, chunkHash, dis.readInt(), dis.readInt(),
				dis.readInt(), dis.readInt(), dis.readInt(), dis.readInt());
	}
}
