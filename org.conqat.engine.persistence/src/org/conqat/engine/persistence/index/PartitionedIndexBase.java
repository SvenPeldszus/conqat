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

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.lib.commons.collections.ListMap;
import org.conqat.lib.commons.collections.PairList;

/**
 * Base class for partitioned indexes.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48662 $
 * @ConQAT.Rating GREEN Hash: 785D5A1CC8096BC290EC7AED804479A5
 */
public class PartitionedIndexBase extends IndexBase {

	/** The separator to separate the partition and the uniform path in the key */
	public static final String KEY_SEPARATOR = "#!#";

	/** Constructor. */
	protected PartitionedIndexBase(IStore store) {
		super(store);
	}

	/**
	 * Splits the keys (obtained by a delta from this index) into a
	 * {@link PairList} in which the first entry denotes the partition and the
	 * second entry the uniform path.
	 */
	public static ListMap<String, String> splitKeys(List<String> keys) {
		ListMap<String, String> result = new ListMap<String, String>();
		for (String key : keys) {
			String[] parts = key.split(KEY_SEPARATOR, 2);
			result.add(parts[0], parts[1]);
		}
		return result;
	}

	/**
	 * Constructs the keys from the given uniform paths by prepending the
	 * partition.
	 */
	protected List<String> makeKeys(String partition, List<String> uniformPaths) {
		List<String> keys = new ArrayList<String>(uniformPaths.size());
		for (String uniformPath : uniformPaths) {
			keys.add(makeKey(partition, uniformPath));
		}
		return keys;
	}

	/** Constructs the key from the given partition and uniform path */
	protected String makeKey(String partition, String uniformPath) {
		return partition + KEY_SEPARATOR + uniformPath;
	}
}
