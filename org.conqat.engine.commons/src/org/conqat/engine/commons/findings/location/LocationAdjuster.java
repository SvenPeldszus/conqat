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
package org.conqat.engine.commons.findings.location;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.commons.findings.DetachedFinding;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.algo.Diff;
import org.conqat.lib.commons.algo.Diff.Delta;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.commons.region.SimpleRegion;
import org.conqat.lib.commons.string.LineOffsetConverter;

/**
 * This class is used for adjusting the offsets used in locations (i.e.
 * subclasses of {@link ElementLocation} for text that is slightly modified. The
 * main use-case is the update of locations where the local (adjusted) text has
 * different line ending, different content due to keyword expansion, or minor
 * local modifications compared to the text on which the analysis was executed
 * (original text).
 * 
 * Both the original and adjusted text may have arbitrary line endings.
 * 
 * The implementation is based on a token diff, which can lead to minor
 * deviations for offsets that are not aligned with token boundaries. A
 * character diff would be more precise, but is too performance and memory
 * intensive for large files.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50376 $
 * @ConQAT.Rating GREEN Hash: DC74AA928456511B8F8621B998BC6629
 */
public class LocationAdjuster {

	/**
	 * If the number of tokens in the adjusted region differs by the tokens in
	 * the original region by more than this factor, the mapping is counted as
	 * wrong.
	 */
	private static final double LOSS_FACTOR = 2;

	/** Maximal number of tokens in the diff we accept. */
	private static final int MAX_DIFF_SIZE = 5000;

	/**
	 * Pattern defining tokens for the diff. Matches either alphanumeric strings
	 * (typical identifiers), or single non-whitespace characters.
	 */
	private static final Pattern TOKEN_PATTERN = Pattern
			.compile("[a-zA-Z0-9_]+|\\S");

	/** The tokens of the original string. */
	private final List<AdjusterToken> originalTokens;

	/**
	 * Adjusted tokens corresponding to the {@link #originalTokens}. If there is
	 * no corresponding token, this list contains null at the index. If the
	 * content could not be matched/adjusted at all (too many differences), this
	 * field is null.
	 */
	private final List<AdjusterToken> mappedAdjustedTokens;

	/** Line offset converted for the original text. */
	private final LineOffsetConverter originalLineOffsetConverter;

	/** Line offset converted for the adjusted text. */
	private final LineOffsetConverter adjustedLineOffsetConverter;

	/**
	 * Constructor.
	 * 
	 * @param originalText
	 *            the text for which the input locations have been created, i.e.
	 *            the text from the analysis.
	 * @param adjustedText
	 *            the text for which the locations should be adjusted, i.e. the
	 *            local text.
	 */
	public LocationAdjuster(String originalText, String adjustedText) {

		originalLineOffsetConverter = new LineOffsetConverter(originalText);
		adjustedLineOffsetConverter = new LineOffsetConverter(adjustedText);
		originalTokens = toTokens(originalText);
		mappedAdjustedTokens = calculateMappedAdjustedTokens(adjustedText,
				originalTokens);
	}

	/**
	 * Calculates the #mappedAdjustedTokens based on original tokens and
	 * adjusted text. May return null if adjustment is not possible due too many
	 * changes.
	 */
	private static List<AdjusterToken> calculateMappedAdjustedTokens(
			String adjustedText, List<AdjusterToken> originalTokens) {

		List<AdjusterToken> adjustedTokens = toTokens(adjustedText);
		Delta<AdjusterToken> delta = Diff.computeDelta(originalTokens,
				adjustedTokens, MAX_DIFF_SIZE);

		if (delta.getSize() >= MAX_DIFF_SIZE) {
			return null;
		}

		return calculateMappedAdjustedTokensFromDelta(delta, originalTokens,
				adjustedTokens);
	}

	/**
	 * Calculates the #mappedAdjustedTokens based on original tokens, adjusted
	 * text, and delta.
	 */
	private static List<AdjusterToken> calculateMappedAdjustedTokensFromDelta(
			Delta<AdjusterToken> delta, List<AdjusterToken> originalTokens,
			List<AdjusterToken> adjustedTokens) {
		List<AdjusterToken> mappedAdjustedTokens = new ArrayList<>(
				Collections.nCopies(originalTokens.size(), (AdjusterToken) null));
		int originalIndex = 0;
		int adjustedIndex = 0;
		for (int i = 0; i < delta.getSize(); ++i) {
			int position = delta.getPosition(i);
			if (position > 0) {
				position -= 1;

				while (adjustedIndex < position) {
					mappedAdjustedTokens.set(originalIndex++,
							adjustedTokens.get(adjustedIndex++));
				}
				adjustedIndex += 1;
			} else {
				position = -position - 1;

				while (originalIndex < position) {
					mappedAdjustedTokens.set(originalIndex++,
							adjustedTokens.get(adjustedIndex++));
				}
				originalIndex += 1;
			}
		}

		while (originalIndex < originalTokens.size()) {
			mappedAdjustedTokens.set(originalIndex++,
					adjustedTokens.get(adjustedIndex++));
		}

		return mappedAdjustedTokens;
	}

	/** Splits a string into tokens. */
	private static List<AdjusterToken> toTokens(String s) {
		List<AdjusterToken> tokens = new ArrayList<AdjusterToken>();
		Matcher matcher = TOKEN_PATTERN.matcher(s);
		while (matcher.find()) {
			tokens.add(new AdjusterToken(matcher.group(), matcher.start()));
		}
		return tokens;
	}

	/**
	 * Maps a zero-based offset range (both inclusive) to the adjusted string.
	 * Returns null if the region could not be approximately mapped.
	 */
	public Region getAdjustedRegion(int originalStartOffset,
			int originalEndOffset) {

		if (mappedAdjustedTokens == null) {
			return null;
		}

		Region originalIndexRegion = findOriginalIndexRegion(
				originalStartOffset, originalEndOffset);
		if (originalIndexRegion.isEmpty()) {
			return null;
		}

		int numOriginalTokens = originalIndexRegion.getLength();
		int numAdjustedTokens = 0;

		AdjusterToken firstAdjustedToken = null;
		AdjusterToken lastAdjustedToken = null;
		for (int i = originalIndexRegion.getStart(); i <= originalIndexRegion
				.getEnd(); ++i) {
			AdjusterToken adjustedToken = mappedAdjustedTokens.get(i);
			if (adjustedToken != null) {
				numAdjustedTokens += 1;
				if (firstAdjustedToken == null) {
					firstAdjustedToken = adjustedToken;
				}
				lastAdjustedToken = adjustedToken;
			}
		}

		if (firstAdjustedToken == null || lastAdjustedToken == null
				|| LOSS_FACTOR * numAdjustedTokens < numOriginalTokens) {
			return null;
		}

		return new Region(firstAdjustedToken.startOffset,
				lastAdjustedToken.endOffset);
	}

	/**
	 * Returns the region of indexes in the {@link #originalTokens} contained in
	 * the given offsets.
	 */
	private Region findOriginalIndexRegion(int originalStartOffset,
			int originalEndOffset) {
		AdjusterToken searchToken = new AdjusterToken(null,
				originalStartOffset, originalEndOffset);

		int originalStartTokenIndex = Collections.binarySearch(originalTokens,
				searchToken, AdjusterToken.COMPARE_BY_START_OFFSET);
		if (originalStartTokenIndex < 0) {
			originalStartTokenIndex = -originalStartTokenIndex - 1;
		}

		int originalEndTokenIndex = Collections.binarySearch(originalTokens,
				searchToken, AdjusterToken.COMPARE_BY_END_OFFSET);
		if (originalEndTokenIndex < 0) {
			// we want insertion point -1
			originalEndTokenIndex = -originalEndTokenIndex - 2;
		}

		return new Region(originalStartTokenIndex, originalEndTokenIndex);
	}

	/**
	 * Returns a new location with adjusted offsets (if necessary). Returns null
	 * if the location does not exist anymore.
	 */
	public ElementLocation adjustLocation(ElementLocation location) {
		if (location instanceof TextRegionLocation) {
			return adjustLocation((TextRegionLocation) location);
		}
		// other locations do not have offsets
		return location;
	}

	/**
	 * Returns a new location with adjusted offsets (if necessary). Returns null
	 * if the location does not exist anymore.
	 */
	public TextRegionLocation adjustLocation(TextRegionLocation location) {
		Region adjustedOffsets = getAdjustedRegion(
				location.getRawStartOffset(), location.getRawEndOffset());

		if (adjustedOffsets == null || adjustedOffsets.isEmpty()) {
			return null;
		}

		int newStartOffset = adjustedOffsets.getStart();
		int newEndOffset = adjustedOffsets.getEnd();
		return new TextRegionLocation(location.getLocation(),
				location.getUniformPath(), newStartOffset, newEndOffset,
				adjustedLineOffsetConverter.getLine(newStartOffset),
				adjustedLineOffsetConverter.getLine(newEndOffset));
	}

	/**
	 * Adjusts all findings in the given list by replacing their location with
	 * the adjusted location (i.e. the findings are modified). If a location
	 * does not exist anymore, the corresponding finding will be excluded from
	 * the returned list.
	 * 
	 * @param findings
	 *            the findings to adjust (the findings will be modified).
	 * 
	 * @return the adjusted findings (only those whose location still exists).
	 */
	public <T extends DetachedFinding> ArrayList<T> adjustFindingLocations(
			Collection<T> findings) {
		ArrayList<T> newFindings = new ArrayList<T>();
		for (T finding : findings) {
			ElementLocation newLocation = adjustLocation(finding.getLocation());
			if (newLocation != null) {
				finding.setLocation(newLocation);
				newFindings.add(finding);
			}
		}
		return newFindings;
	}

	/**
	 * Adjusts the location of a single line. This only respects the token part
	 * of a line, i.e. leading and trailing whitespace of a line will be
	 * ignored.
	 * 
	 * @param line
	 *            the one-based line number of be adjusted.
	 * @return the one-based lines encoded as a region, as a line may map to
	 *         multiple lines after changing. This may also return null, if no
	 *         non-empty lines could be found that correspond to the input line
	 *         after adjustment.
	 * @throws ConQATException
	 *             if the given line is invalid for the original text
	 */
	public SimpleRegion adjustLine(int line) throws ConQATException {
		if (!originalLineOffsetConverter.isValidLine(line)) {
			throw new ConQATException("Invalid line number: " + line);
		}
		int originalStartOffset = originalLineOffsetConverter.getOffset(line);
		int originalEndOffset = originalLineOffsetConverter.getOffset(line + 1) - 1;
		Region adjustedOffsets = getAdjustedRegion(originalStartOffset,
				originalEndOffset);
		if (adjustedOffsets == null) {
			return null;
		}

		int adjustedStartLine = adjustedLineOffsetConverter
				.getLine(adjustedOffsets.getStart());
		int adjustedEndLine = adjustedLineOffsetConverter
				.getLine(adjustedOffsets.getEnd());
		return new SimpleRegion(adjustedStartLine, adjustedEndLine);
	}

	/** Simple token representation used in location adjustment. */
	private static class AdjusterToken {

		/** Compares by start offset. */
		private static final Comparator<AdjusterToken> COMPARE_BY_START_OFFSET = new Comparator<AdjusterToken>() {
			@Override
			public int compare(AdjusterToken token1, AdjusterToken token2) {
				return token1.startOffset - token2.startOffset;
			}
		};

		/** Compares by end offset. */
		private static final Comparator<AdjusterToken> COMPARE_BY_END_OFFSET = new Comparator<AdjusterToken>() {
			@Override
			public int compare(AdjusterToken token1, AdjusterToken token2) {
				return token1.endOffset - token2.endOffset;
			}
		};

		/** The text content. */
		private final String text;

		/** The start offset in the text. */
		private final int startOffset;

		/** The inclusive end offset in the text. */
		private final int endOffset;

		/** Constructor. */
		public AdjusterToken(String text, int startOffset) {
			this(text, startOffset, startOffset + text.length() - 1);
		}

		/** Constructor. */
		public AdjusterToken(String text, int startOffset, int endOffset) {
			this.text = text;
			this.startOffset = startOffset;
			this.endOffset = endOffset;
		}

		/** {@inheritDoc} */
		@Override
		public boolean equals(Object obj) {
			return (obj instanceof AdjusterToken)
					&& ((AdjusterToken) obj).text.equals(text);
		}

		/** {@inheritDoc} */
		@Override
		public int hashCode() {
			return text.hashCode();
		}
	}
}
