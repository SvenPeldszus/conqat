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

import java.io.Serializable;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.util.StorageUtils;

/**
 * Abstract base class for {@link ValueIndexBase} where the byte conversion is
 * just performed by Java serialization.
 * 
 * @param <T>
 *            the type stored as values.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47454 $
 * @ConQAT.Rating GREEN Hash: BE6302EC178EF6A080100800728D470E
 */
public abstract class SerializationBasedValueIndexBase<T extends Serializable>
		extends ValueIndexBase<T> {

	/** Constructor. */
	protected SerializationBasedValueIndexBase(IStore store) {
		super(store);
	}

	/** {@inheritDoc} */
	@Override
	protected byte[] valueToByteArray(T value) throws StorageException {
		return StorageUtils.serialize(value);
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unchecked")
	@Override
	protected T byteArrayToValue(byte[] bytes) throws StorageException {
		return (T) StorageUtils.deserialize(bytes);
	}
}
