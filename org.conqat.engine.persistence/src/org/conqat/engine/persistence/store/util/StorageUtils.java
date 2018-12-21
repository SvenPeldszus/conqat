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
package org.conqat.engine.persistence.store.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.IStorageSystem;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.io.SerializationUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Utility methods used for dealing with the storage system.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51583 $
 * @ConQAT.Rating GREEN Hash: 840FC3AB2E14609DD1EDFE2206F020FD
 */
public class StorageUtils {

	/** Integer value used to terminate the last entry in exported stores. */
	private static final int RECORD_TERMINATOR = -1;

	/**
	 * The minimal number of bytes that must be read in
	 * {@link #importStore(IStore, DataInputStream)} before a batch put
	 * operation is performed.
	 */
	private static final int MIN_READ_BYTES = 1024 * 1024;

	/** Returns the list of all keys as strings for the given store. */
	public static List<String> listStringKeys(IStore store)
			throws StorageException {
		final List<String> result = new ArrayList<String>();
		store.scan(new byte[0], new IKeyValueCallback() {
			@Override
			public void callback(byte[] key, byte[] value) {
				synchronized (result) {
					result.add(StringUtils.bytesToString(key));
				}
			}
		});
		return result;
	}

	/** Returns the list of all keys for the given store. */
	public static List<byte[]> listKeys(IStore store) throws StorageException {
		return listKeysStartingWith(new byte[0], store);
	}

	/** Returns all keys starting with the given prefix. */
	public static List<byte[]> listKeysStartingWith(String prefix, IStore store)
			throws StorageException {
		return listKeysStartingWith(StringUtils.stringToBytes(prefix), store);
	}

	/** Returns all keys starting with the given prefix. */
	public static List<byte[]> listKeysStartingWith(byte[] prefix, IStore store)
			throws StorageException {
		List<byte[]> keys = new ArrayList<byte[]>();
		KeyCollectingCallback callback = new KeyCollectingCallback(keys);
		store.scanKeys(prefix, callback);
		return keys;
	}

	/** Returns the number of keys for the given store. */
	public static int keyCount(IStore store) throws StorageException {
		KeyCountingCallback callback = new KeyCountingCallback();
		store.scanKeys(new byte[0], callback);
		return callback.getNumberOfKeys();
	}

	/** Completely erases the contents of the given store */
	public static void clearStore(IStore store) throws StorageException {
		store.removeByPrefix(new byte[0]);
	}

	/**
	 * Completely erases the contents of the store with the specified name in
	 * the given storage system
	 */
	public static void clearStore(IStorageSystem storageSystem, String store)
			throws StorageException {
		clearStore(storageSystem.openStore(store));
	}

	/**
	 * Exports the entire contents of a store into an output stream. The format
	 * consists for each key of the length of the key (int), the key bytes, the
	 * length of the value (int), the value bytes. After the last entry, the
	 * {@link #RECORD_TERMINATOR} is inserted to mark the end of entries.
	 * 
	 * @return the number of records written into the stream.
	 */
	public static int exportStore(IStore store, final DataOutputStream out)
			throws StorageException, IOException {
		StreamWritingCallback callback = new StreamWritingCallback(out);
		store.scan(new byte[0], callback);
		if (callback.exception != null) {
			throw callback.exception;
		}
		out.writeInt(RECORD_TERMINATOR);
		return callback.recordCount;
	}

	/** Serializes an entire store into a byte array. */
	public static byte[] exportStoreToBytes(IStore store)
			throws StorageException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try (DataOutputStream dos = new DataOutputStream(bos)) {
			StorageUtils.exportStore(store, dos);
		} catch (IOException e) {
			throw new AssertionError(
					"This should not happen, as we work in memory!", e);
		}
		return bos.toByteArray();
	}

	/**
	 * Imports the data from a stream into a store. The store must have the
	 * format described in {@link #exportStore(IStore, DataOutputStream)}. The
	 * store will not be initially cleared. If this is required, call
	 * {@link #clearStore(IStore)} before. Any existing value that collides with
	 * an entry found in the stream will be silently overwritten.
	 * 
	 * @return the number of records read from the stream.
	 */
	public static int importStore(IStore store, DataInputStream in)
			throws IOException, StorageException {
		int recordCount = 0;
		int byteCount = 0;

		PairList<byte[], byte[]> data = new PairList<byte[], byte[]>();

		while (true) {
			int keySize = in.readInt();
			if (keySize == RECORD_TERMINATOR) {
				store.put(data);
				return recordCount;
			}
			byte[] key = new byte[keySize];
			FileSystemUtils.safeRead(in, key);

			int valueSize = in.readInt();
			byte[] value = new byte[valueSize];
			FileSystemUtils.safeRead(in, value);

			data.add(key, value);
			recordCount += 1;

			byteCount += keySize + valueSize;
			// commit the data in batches of at least MIN_READ_BYTES
			if (byteCount >= MIN_READ_BYTES) {
				store.put(data);
				byteCount = 0;
				data.clear();
			}
		}
	}

	/**
	 * Replaces the contents of the given store with data read from a byte
	 * array. The byte array must be created with
	 * {@link #exportStoreToBytes(IStore)}.
	 */
	public static void replaceStoreContentFromBytes(IStore store, byte[] data)
			throws StorageException {
		clearStore(store);
		try {
			importStore(store, new DataInputStream(new ByteArrayInputStream(
					data)));
		} catch (IOException e) {
			throw new AssertionError(
					"This should not happen, as we work in memory!", e);
		}
	}

	/**
	 * Callback that performs writing into the output stream. The format follows
	 * the description from exportStore() method.
	 */
	private static final class StreamWritingCallback implements
			IKeyValueCallback {

		/** The stream to write into. */
		private final DataOutputStream outputStream;

		/** The exception caught during callback handling (if any). */
		private IOException exception;

		/** Number of records written. */
		private int recordCount = 0;

		/** Constructor. */
		private StreamWritingCallback(DataOutputStream outputStream) {
			this.outputStream = outputStream;
		}

		/** {@inheritDoc} */
		@Override
		public void callback(byte[] key, byte[] value) {
			try {
				synchronized (outputStream) {
					outputStream.writeInt(key.length);
					outputStream.write(key);
					outputStream.writeInt(value.length);
					outputStream.write(value);
					++recordCount;
				}
			} catch (IOException e) {
				exception = e;
			}
		}
	}

	/**
	 * Deserializes a byte array, wrapping possible problems in a
	 * {@link StorageException}.
	 */
	public static Serializable deserialize(byte[] value)
			throws StorageException {
		try {
			return SerializationUtils.deserializeFromByteArray(value, Thread
					.currentThread().getContextClassLoader());
		} catch (ClassNotFoundException e) {
			throw new StorageException(e);
		} catch (IOException e) {
			throw new StorageException(e);
		}
	}

	/**
	 * Serializes into a byte array, wrapping possible problems in a
	 * {@link StorageException}.
	 */
	public static byte[] serialize(Serializable value) throws StorageException {
		try {
			return SerializationUtils.serializeToByteArray(value);
		} catch (IOException e) {
			throw new StorageException(e);
		}
	}

	/** Deletes all values between begin (inclusive) and end (exclusive) keys. */
	public static void deleteRange(ConvenientStore store, byte[] begin,
			byte[] end) throws StorageException {
		List<byte[]> keys = new ArrayList<byte[]>();
		KeyCollectingCallback callback = new KeyCollectingCallback(keys);
		store.scanKeys(begin, end, callback);
		store.remove(keys);
	}

	/**
	 * Serializes the store to a readable/comparable string. This is typically
	 * used for testing, as the format is not very compact.
	 */
	public static String serializeStoreForTest(IStore store)
			throws StorageException {
		final List<String> lines = new ArrayList<String>();
		store.scan(new byte[0], new IKeyValueCallback() {
			@Override
			public void callback(byte[] key, byte[] value) {
				lines.add(StringUtils.encodeAsHex(key) + " = "
						+ StringUtils.encodeAsHex(value));
			}
		});
		return StringUtils.concat(CollectionUtils.sort(lines), StringUtils.CR);
	}

}
