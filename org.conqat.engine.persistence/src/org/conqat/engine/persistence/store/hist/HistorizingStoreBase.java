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
package org.conqat.engine.persistence.store.hist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.collections.PairList;

/**
 * Base class of historizing stores. This contains the methods used for key
 * handling.
 * <p>
 * This class makes all write operations throw exceptions to simplify writing
 * read-only stores.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51715 $
 * @ConQAT.Rating GREEN Hash: FD5DD7085C08FFAF07A823EE980B1952
 */
public abstract class HistorizingStoreBase implements IStore {

	/** The string used as prefix for marking the head revision. */
	protected static final byte[] HEAD_PREFIX = "#_HEAD_#".getBytes();

	/** Single 0 byte marks deleted entries. */
	protected static final byte[] DELETION_MARKER = new byte[] { 0 };

	/**
	 * Byte used to separate the key from the revision. This is a value that is
	 * not likely to be in a string, but also not one of the border values (0,
	 * 0xff).
	 */
	protected static final byte TIMESTAMP_SEPARATOR = (byte) 0xfe;

	/** The delegate store. */
	protected final IStore store;

	/** Constructor. */
	protected HistorizingStoreBase(IStore delegate) {
		this.store = delegate;
	}

	/** Makes a head key by prefixing {@link #HEAD_PREFIX}. */
	protected static byte[] headKey(byte[] key) {
		if (key == null) {
			return HEAD_PREFIX;
		}

		byte[] newKey = new byte[key.length + HEAD_PREFIX.length];
		System.arraycopy(HEAD_PREFIX, 0, newKey, 0, HEAD_PREFIX.length);
		System.arraycopy(key, 0, newKey, HEAD_PREFIX.length, key.length);
		return newKey;
	}

	/** Converts a list of keys to head keys using {@link #headKey(byte[])}. */
	protected static List<byte[]> headKeyList(List<byte[]> keys) {
		List<byte[]> result = new ArrayList<byte[]>();
		for (byte[] key : keys) {
			result.add(headKey(key));
		}
		return result;
	}

	/** Strips the head prefix from a key. */
	private static byte[] stripHeadPrefix(byte[] key) {
		byte[] newKey = new byte[key.length - HEAD_PREFIX.length];
		System.arraycopy(key, HEAD_PREFIX.length, newKey, 0, newKey.length);
		return newKey;
	}

	/** {@inheritDoc} */
	@Override
	public void put(byte[] key, byte[] value) throws StorageException {
		throwWritingNotSupported();

	}

	/** {@inheritDoc} */
	@Override
	public void put(PairList<byte[], byte[]> keysValues)
			throws StorageException {
		throwWritingNotSupported();
	}

	/** {@inheritDoc} */
	@Override
	public void remove(byte[] key) throws StorageException {
		throwWritingNotSupported();
	}

	/** {@inheritDoc} */
	@Override
	public void remove(List<byte[]> keys) throws StorageException {
		throwWritingNotSupported();
	}

	/** {@inheritDoc} */
	@Override
	public void removeByPrefix(byte[] prefix) throws StorageException {
		throwWritingNotSupported();
	}

	/** Throws an exception to indicate that writing is not supported. */
	private static void throwWritingNotSupported() throws StorageException {
		throw new StorageException(
				"Operation not supported by this read-only store!");
	}

	/** Makes a key a revision key by appending the timestamp suffix. */
	public static byte[] revisionKey(byte[] key, byte[] timestampSuffix) {
		byte[] newKey = new byte[key.length + 1 + timestampSuffix.length];
		System.arraycopy(key, 0, newKey, 0, key.length);
		newKey[key.length] = TIMESTAMP_SEPARATOR;
		System.arraycopy(timestampSuffix, 0, newKey, key.length + 1,
				timestampSuffix.length);
		return newKey;
	}

	/** Returns whether the value is the deletion marker. */
	public static boolean isDeletionValue(byte[] value) {
		return Arrays.equals(value, DELETION_MARKER);
	}

	/** Callback that forwards the results with the head prefix stripped. */
	protected static final class HeadStrippingCallback implements
			IKeyValueCallback {

		/** Delegate to forward calls to. */
		private final IKeyValueCallback delegate;

		/** Constructor. */
		public HeadStrippingCallback(IKeyValueCallback callback) {
			this.delegate = callback;
		}

		/** {@inheritDoc} */
		@Override
		public void callback(byte[] key, byte[] value) {
			delegate.callback(stripHeadPrefix(key), value);
		}
	}
}
