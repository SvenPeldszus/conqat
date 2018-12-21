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

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.DelegatingStore;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Wrapper class that adds convenience methods to an {@link IStore}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49494 $
 * @ConQAT.Rating GREEN Hash: DF86A92D88905FFDBFA66A759E8C399B
 */
public class ConvenientStore extends DelegatingStore {

	/** Constructor. */
	public ConvenientStore(IStore store) {
		super(store);
	}

	/**
	 * Returns the entry stored for the given string key (or <code>null</code>
	 * if none is found).
	 */
	public byte[] getWithString(String key) throws StorageException {
		return store.get(StringUtils.stringToBytes(key));
	}

	/**
	 * Returns the entries stored for the given string keys (including
	 * <code>null</code> entries for non-existing keys).
	 */
	public List<byte[]> getWithStrings(List<String> keys)
			throws StorageException {
		return store.get(convertKeys(keys));
	}

	/** Converts the list of string keys to byte[] keys. */
	private List<byte[]> convertKeys(List<String> keys) {
		List<byte[]> byteKeys = new ArrayList<byte[]>();
		for (String key : keys) {
			byteKeys.add(StringUtils.stringToBytes(key));
		}
		return byteKeys;
	}

	/** Stores data for the given key. */
	public void putWithString(String key, byte[] value) throws StorageException {
		store.put(StringUtils.stringToBytes(key), value);
	}

	/** Stores data for the given keys. */
	public void putWithStrings(PairList<String, byte[]> keysValues)
			throws StorageException {
		PairList<byte[], byte[]> byteKeysValues = new PairList<byte[], byte[]>(
				keysValues.size());
		for (int i = 0; i < keysValues.size(); ++i) {
			byteKeysValues.add(
					StringUtils.stringToBytes(keysValues.getFirst(i)),
					keysValues.getSecond(i));
		}

		store.put(byteKeysValues);
	}

	/** Removes the entry stored for the given string key. */
	public void removeWithString(String key) throws StorageException {
		store.remove(StringUtils.stringToBytes(key));
	}

	/** Removes the entries stored for the given string keys. */
	public void removeWithStrings(List<String> keys) throws StorageException {
		store.remove(convertKeys(keys));
	}

	/** Scans all entries for the given prefix. */
	public void scan(String prefix, IKeyValueCallback callback)
			throws StorageException {
		store.scan(StringUtils.stringToBytes(prefix), callback);
	}

}