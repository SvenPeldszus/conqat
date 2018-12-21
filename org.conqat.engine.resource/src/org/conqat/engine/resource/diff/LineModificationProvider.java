package org.conqat.engine.resource.diff;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.algo.Diff.Delta;
import org.conqat.lib.commons.collections.CollectionUtils;

/**
 * Provides line modification data based on delta information. E.g. used to
 * color inserted / delete lines in code listings.
 * 
 * @author $Author: pfaller $
 * @version $Rev: 47530 $
 * @ConQAT.Rating RED Hash: DF41971342649EED33ED5BB3F3B06BDA
 */
public class LineModificationProvider {

	/** Inserted lines */
	private final List<Integer> insertions;

	/** Lines before which content was deleted */
	private final List<Integer> deletions;

	/** Flag that indicates deletion */
	private static final int DELETED = -1;

	/** Maps from comparee to main position */
	private final int[] positionMap;

	/** Constructor. */
	public LineModificationProvider(Delta<?> delta) {
		insertions = computeInsertions(delta);
		positionMap = computePositionMap(delta, delta.getN());
		deletions = computeDeletions();
	}

	/** Compute mapping from comparee to main position */
	private static int[] computePositionMap(Delta<?> delta, int baslineCount) {

		// initialize position map
		int[] positionMap = new int[baslineCount];
		for (int i = 0; i < baslineCount; i++) {
			positionMap[i] = i;
		}

		// TODO (BH): This is far more complicated and inefficient than it has
		// to be. The alternative solution is hard to explain in text, but if
		// you want we can co-program it (given the existing tests, this should
		// be save).
		for (int editOperation = 0; editOperation < delta.getSize(); editOperation++) {

			int editPosition = delta.getPosition(editOperation);
			if (editPosition > 0) {
				editPosition -= 1; // correct inc. of 1 of all pos. >0

				// shift right
				for (int i = 0; i < baslineCount; i++) {
					int value = positionMap[i];
					if (value >= editPosition && value != DELETED) {
						positionMap[i] = value + 1;
					}
				}

			} else {
				editPosition = Math.abs(editPosition);
				editPosition -= 1;

				// mark deleted
				positionMap[editPosition] = DELETED;

				// shift all positions behind to the left
				for (int i = editPosition + 1; i < baslineCount; i++) {
					int value = positionMap[i];
					if (value != DELETED) {
						positionMap[i] = value - 1;
					}
				}
			}
		}

		return positionMap;
	}

	/** Compute lines that have been inserted */
	private List<Integer> computeInsertions(Delta<?> delta) {
		List<Integer> insertions = new ArrayList<Integer>();

		for (int pos = 0; pos < delta.getSize(); pos++) {
			int change = delta.getPosition(pos);

			if (change > 0) {
				insertions.add(change - 1);
			}

		}
		return insertions;
	}

	/** Compute lines that have been deleted */
	private List<Integer> computeDeletions() {
		List<Integer> deletions = new ArrayList<Integer>();

		boolean pendingDeletion = false;
		for (int baslinePosition = 0; baslinePosition < positionMap.length; baslinePosition++) {
			if (positionMap[baslinePosition] == DELETED) {
				pendingDeletion = true;
			} else if (pendingDeletion) {
				int mainPosition = positionMap[baslinePosition];
				deletions.add(mainPosition);
				pendingDeletion = false;
			}
		}

		return deletions;
	}

	/** Get inserted lines */
	public List<Integer> getInsertions() {
		return CollectionUtils.asUnmodifiable(insertions);
	}

	/** Get deleted lines */
	public List<Integer> getDeletions() {
		return CollectionUtils.asUnmodifiable(deletions);
	}

	/**
	 * Maps from an comparee line position to the main position of that line. If
	 * the line was deleted, {@link #DELETED} is returned.
	 * 
	 * @throws ConQATException
	 *             if comparee position is not a valid line
	 */
	public int mapBaslineTomainPosition(int compareePosition)
			throws ConQATException {
		if (compareePosition < 0 || compareePosition >= positionMap.length) {
			throw new ConQATException("Line position " + compareePosition
					+ " not valid in comparee.");
		}
		return positionMap[compareePosition];

	}

	/**
	 * Checks if the line at the given position was inserted.
	 */
	public boolean isInserted(int position) {
		return insertions.contains(position);
	}

	/**
	 * Checks if the region of the main location is a sub-region of the comparee
	 * location.
	 */
	public boolean overlaps(TextRegionLocation mainLocation,
			TextRegionLocation compareeLocation) throws ConQATException {

		int mainStart = mainLocation.getRawStartLine();
		int mainEnd = mainLocation.getRawEndLine();

		int compareeStart = mapBaslineTomainPosition(compareeLocation
				.getRawStartLine());
		int compareeEnd = mapBaslineTomainPosition(compareeLocation
				.getRawEndLine());

		return compareeStart <= mainStart && compareeEnd >= mainEnd;
	}

	/**
	 * Checks whether a modification was done right after the given line.
	 */
	public boolean isModifiedAfter(int line) {
		return isInserted(line) || isInserted(line + 1) || isInserted(line + 2);
	}

	/**
	 * Checks whether a modification was done right before the given line.
	 */
	public boolean isModifiedBefore(int line) {
		return isInserted(line - 2) || isInserted(line - 3)
				|| isInserted(line - 4);
	}

}
