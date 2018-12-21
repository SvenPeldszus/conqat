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
package org.conqat.engine.code_clones.index;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.code_clones.core.Clone;
import org.conqat.engine.code_clones.core.CloneClass;
import org.conqat.engine.code_clones.detection.suffixtree.CloneDetectingSuffixTree;
import org.conqat.engine.code_clones.detection.suffixtree.ICloneReporter;
import org.conqat.engine.code_clones.index.report.ICloneClassReporter;
import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.digest.Digester;

/**
 * This class implements the core search algorithm of the index-based clone
 * detection approach. This searches for clones in the string created by chunks
 * of the origin file and all chunks with the same hash code as chunks in this
 * file. The search is performed using a suffix tree based clone detection.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 48919 $
 * @ConQAT.Rating GREEN Hash: 07D656650C46B240C5F77C4ED3DC315F
 */
public class CloneIndexCloneSearcher implements ICloneReporter {

	/** The chunk length used. */
	private final int chunkLength;

	/** The origin ID of the file currently searched for clones. */
	private final String originId;

	/** The reported used for clone class construction/reporting. */
	private final ICloneClassReporter reporter;

	/**
	 * If this is true, only clone classes for which this origin contributes the
	 * first clone instance are reported. If this is false, all clone classes
	 * are reported. This should be set to true when querying all origins
	 * consecutively to avoid duplicate clone groups.
	 */
	private final boolean onlyStartingHere;

	/** Minimal length of reported clones. */
	private final int minLength;

	/**
	 * The string of chunks including sentinels between nonconsecutive
	 * substrings.
	 */
	private final List<SuffixTreeChunkWrapper> chunkString = new ArrayList<>();

	/** The clone class currently constructed and to be reported. */
	private CloneClass cloneClass;

	/**
	 * Flag for storing whether the {@link #originId} was found during creation
	 * of the clone class. This is used to filter clone classes between other
	 * files that are "accidentially" found during construction. These are not
	 * relevant and likely to be incomplete, as only part of the file is
	 * present.
	 */
	private boolean hadOriginIdDuringCloneClassConstruction = false;

	/** Constructor. */
	public CloneIndexCloneSearcher(String originId,
			ICloneClassReporter reporter, boolean onlyStartingHere,
			int minLength, List<Chunk> chunks, int chunkLength) {
		this.originId = originId;
		this.reporter = reporter;
		this.onlyStartingHere = onlyStartingHere;
		this.minLength = minLength;
		this.chunkLength = chunkLength;

		fillChunkString(chunks);
	}

	/**
	 * Fills the {@link #chunkString} from the given list of chunks, ordering
	 * chunks by element and position and separating non-consecutive substrings
	 * with a sentinel.
	 */
	private void fillChunkString(List<Chunk> chunks) {
		Chunk previous = null;
		for (Chunk chunk : CollectionUtils.sort(chunks,
				ChunkUtils.CHUNK_COMPARATOR)) {
			if (previous != null && !isConsecutive(previous, chunk)) {
				chunkString.add(SuffixTreeChunkWrapper.createSentinel());
			}
			chunkString.add(new SuffixTreeChunkWrapper(chunk));
			previous = chunk;
		}
		chunkString.add(SuffixTreeChunkWrapper.createSentinel());
	}

	/**
	 * Returns whether the two (non-null) chunks are in the same file and
	 * immediately follow each other.
	 */
	private static boolean isConsecutive(Chunk previous, Chunk chunk) {
		if (!previous.getOriginId().equals(chunk.getOriginId())) {
			return false;
		}
		return previous.getFirstUnitIndex() + 1 == chunk.getFirstUnitIndex();
	}

	/** Reports clones based on a list of ordered chunks. */
	public void reportClones() throws StorageException, ConQATException {
		new CloneDetectingSuffixTree(chunkString).findClones(minLength
				- chunkLength + 1, this);
	}

	/** {@inheritDoc} */
	@Override
	public void startCloneClass(int normalizedLength) {
		cloneClass = new CloneClass(normalizedLength + chunkLength - 1,
				reporter.provideId());
		hadOriginIdDuringCloneClassConstruction = false;
	}

	/** {@inheritDoc} */
	@Override
	public Clone addClone(int startPosition, int length) {
		Chunk headChunk = chunkString.get(startPosition).getChunk();
		Chunk tailChunk = chunkString.get(startPosition + length - 1)
				.getChunk();

		hadOriginIdDuringCloneClassConstruction = hadOriginIdDuringCloneClassConstruction
				|| headChunk.getOriginId().equals(originId);

		TextRegionLocation location = new TextRegionLocation(
				headChunk.getOriginId(), headChunk.getOriginId(),
				headChunk.getRawStartOffset(), tailChunk.getRawEndOffset() - 1,
				headChunk.getFirstRawLineNumber(),
				tailChunk.getLastRawLineNumber() - 1);

		// We respect only the head and tail chunks for the fingerprint.
		// So if there are different clones where the first and last 5 (or
		// chunk-length) units are the same, they will have the same
		// fingerprint. This is considered unlikely.
		String fingerprintBase = headChunk.getChunkHash().toString()
				+ tailChunk.getChunkHash().toString();
		String fingerprint = Digester.createMD5Digest(fingerprintBase);

		return new Clone(reporter.provideId(), cloneClass, location,
				headChunk.getFirstUnitIndex(), length + chunkLength - 1,
				fingerprint);
	}

	/** {@inheritDoc} */
	@Override
	public boolean completeCloneClass() throws ConQATException {
		if (!hadOriginIdDuringCloneClassConstruction) {
			return false;
		}

		if (onlyStartingHere
				&& !originId.equals(getMinimalOriginId(cloneClass))) {
			return false;
		}

		reporter.report(cloneClass);
		return true;
	}

	/**
	 * Returns the lexicographically smallest origin id of the given clone
	 * class.
	 */
	private static String getMinimalOriginId(CloneClass cloneClass) {
		String minimalOriginId = null;
		for (Clone clone : cloneClass.getClones()) {
			String originId = clone.getLocation().getUniformPath();
			if (minimalOriginId == null
					|| originId.compareTo(minimalOriginId) < 0) {
				minimalOriginId = originId;
			}
		}
		return minimalOriginId;
	}

}