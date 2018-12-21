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
package org.conqat.engine.persistence.index.schema;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Set;

import org.conqat.engine.persistence.index.IndexBase;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableSet;

/**
 * Description of a single entry in the schema.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51716 $
 * @ConQAT.Rating GREEN Hash: 94CBB345219EB1D80EED924961EC2D61
 */
public class SchemaEntry implements Serializable {

	/**
	 * This is a (temporary) way to disable branching stores in case they are
	 * buggy in production.
	 */
	private static final boolean DISABLE_BRANCHING_STORES = Boolean
			.getBoolean("com.teamscale.disable-branching-store");

	/** Serial version UID. */
	private static final long serialVersionUID = 1;

	/**
	 * The class of the index that defines the data format. This is stored as a
	 * string to avoid classloader problems during deserialization.
	 * Additionally, this is only used for comparison.
	 */
	private final String indexClass;

	/** Options to be used for the underlying store. */
	private final Set<EStorageOption> storageOptions = EnumSet
			.noneOf(EStorageOption.class);

	/** Constructor. */
	/* package */SchemaEntry(String indexClassName,
			Collection<EStorageOption> storageOptions) {
		this.indexClass = indexClassName;
		this.storageOptions.addAll(storageOptions);

		if (this.storageOptions.contains(EStorageOption.HISTORIZED)
				&& this.storageOptions.contains(EStorageOption.BRANCHED)) {
			throw new IllegalArgumentException(
					"Can not create a store that is both historized and branched!");
		}

		// map branched stores to historized ones as a fallback
		if (DISABLE_BRANCHING_STORES
				&& this.storageOptions.contains(EStorageOption.BRANCHED)) {
			this.storageOptions.remove(EStorageOption.BRANCHED);
			this.storageOptions.add(EStorageOption.HISTORIZED);
		}
	}

	/** Constructor. */
	public SchemaEntry(Class<? extends IndexBase> indexClass,
			Collection<EStorageOption> storageOptions) {
		this(indexClass.getName(), storageOptions);
	}

	/** Constructor. */
	public SchemaEntry(Class<? extends IndexBase> indexClass,
			EStorageOption... storageOptions) {
		this(indexClass, Arrays.asList(storageOptions));
	}

	/** Returns the index class. */
	public String getIndexClass() {
		return indexClass;
	}

	/** Returns the storage options. */
	public UnmodifiableSet<EStorageOption> getStorageOptions() {
		return CollectionUtils.asUnmodifiable(storageOptions);
	}

	/**
	 * Returns whether the given option should be used for the underlying store.
	 */
	public boolean usesOption(EStorageOption option) {
		return storageOptions.contains(option);
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((indexClass == null) ? 0 : indexClass.hashCode());
		result = prime * result
				+ ((storageOptions == null) ? 0 : storageOptions.hashCode());
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object other) {
		if (!(other instanceof SchemaEntry)) {
			return false;
		}
		SchemaEntry otherEntry = (SchemaEntry) other;
		return indexClass.equals(otherEntry.indexClass)
				&& storageOptions.equals(otherEntry.storageOptions);
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return indexClass + " (" + storageOptions.toString() + ")";
	}

	/** Returns whether this entry uses some kind of historization layer. */
	public boolean isHistorized() {
		return usesOption(EStorageOption.HISTORIZED)
				|| usesOption(EStorageOption.BRANCHED);
	}
}