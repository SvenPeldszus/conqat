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
package org.conqat.engine.code_clones.index;

import org.conqat.engine.code_clones.detection.suffixtree.SuffixTree;

/**
 * {@link SuffixTree} {@link Chunk} wrapper, used to implement a hash based
 * {@link #equals(Object)} on Chunks.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 48919 $
 * @ConQAT.Rating GREEN Hash: AFC7C9335C7DB334FFC5877134DD41F5
 */
public class SuffixTreeChunkWrapper {

	/** The original chunk. If this is null, this marks a sentinel. */
	private final Chunk chunk;

	/** Constructor */
	public SuffixTreeChunkWrapper(Chunk originalChunk) {
		chunk = originalChunk;
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object other) {
		if (other instanceof SuffixTreeChunkWrapper) {
			SuffixTreeChunkWrapper otherChunk = (SuffixTreeChunkWrapper) other;
			if (isSentinel() || otherChunk.isSentinel()) {
				return this == other;
			}
			return otherChunk.chunk.getChunkHash().equals(chunk.getChunkHash());
		}
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		if (isSentinel()) {
			return 0;
		}
		return chunk.getChunkHash().hashCode();
	}

	/** Returns chunk (may be null for sentinels). */
	public Chunk getChunk() {
		return chunk;
	}

	/** Returns whether this is a sentinel. */
	public boolean isSentinel() {
		return chunk == null;
	}

	/** Returns a new sentinel. */
	public static SuffixTreeChunkWrapper createSentinel() {
		return new SuffixTreeChunkWrapper(null);
	}
}
