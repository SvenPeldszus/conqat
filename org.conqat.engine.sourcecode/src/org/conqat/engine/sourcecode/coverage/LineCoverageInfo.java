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
package org.conqat.engine.sourcecode.coverage;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Holds line coverage information for a file.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51014 $
 * @ConQAT.Rating GREEN Hash: 701F2426EEBB1EDF2FE11EF497966E33
 */
public class LineCoverageInfo implements Serializable {

	/** Version for serialization. */
	private static final long serialVersionUID = 1L;

	/** The line numbers that were fully covered */
	private final Set<Integer> fullyCoveredLines = new HashSet<>();

	/** The line numbers that were partially covered */
	private final Set<Integer> partiallyCoveredLines = new HashSet<>();

	/** The line numbers that were not covered */
	private final Set<Integer> uncoveredLines = new HashSet<>();

	/**
	 * Adds the coverage information for the given line. This merges the given
	 * coverage info if called multiple times for the same line. This is needed
	 * to allow for overlapping coverage reports.
	 */
	public void addLineCoverage(int line, ELineCoverage coverage) {
		CCSMAssert.isNotNull(coverage);

		ELineCoverage existingCoverage = getLineCoverage(line);
		if (existingCoverage == null) {
			setLineCoverage(line, coverage);
			return;
		}

		switch (existingCoverage) {
		case NOT_COVERED:
			uncoveredLines.remove(line);
			setLineCoverage(line, coverage);
			break;
		case PARTIALLY_COVERED:
			if (coverage.equals(ELineCoverage.FULLY_COVERED)) {
				partiallyCoveredLines.remove(line);
				fullyCoveredLines.add(line);
			}
			break;
		case FULLY_COVERED:
			// cannot get any better
			break;
		default:
			throw new IllegalStateException("Unknown line coverage: "
					+ coverage);
		}

	}

	/**
	 * Sets the line coverage for the given line. This ignores previously stored
	 * values.
	 */
	private void setLineCoverage(int line, ELineCoverage coverage) {
		switch (coverage) {
		case FULLY_COVERED:
			fullyCoveredLines.add(line);
			break;
		case PARTIALLY_COVERED:
			partiallyCoveredLines.add(line);
			break;
		case NOT_COVERED:
			uncoveredLines.add(line);
			break;
		default:
			throw new IllegalStateException("Unknown line coverage: "
					+ coverage);
		}
	}

	/** Adds all coverage information from another {@link LineCoverageInfo}. */
	public void addAll(LineCoverageInfo coverageInfo) {
		for (int line : coverageInfo.fullyCoveredLines) {
			addLineCoverage(line, ELineCoverage.FULLY_COVERED);
		}
		for (int line : coverageInfo.partiallyCoveredLines) {
			addLineCoverage(line, ELineCoverage.PARTIALLY_COVERED);
		}
		for (int line : coverageInfo.uncoveredLines) {
			addLineCoverage(line, ELineCoverage.NOT_COVERED);
		}
	}

	/**
	 * Returns the line coverage for the given line or <code>null</code> if none
	 * is stored.
	 */
	public ELineCoverage getLineCoverage(int line) {
		if (fullyCoveredLines.contains(line)) {
			return ELineCoverage.FULLY_COVERED;
		}
		if (partiallyCoveredLines.contains(line)) {
			return ELineCoverage.PARTIALLY_COVERED;
		}
		if (uncoveredLines.contains(line)) {
			return ELineCoverage.NOT_COVERED;
		}
		return null;
	}

	/** Returns list of fully covered lines (sorted ascending) */
	public List<Integer> getFullyCoveredLines() {
		return CollectionUtils.sort(fullyCoveredLines);
	}

	/** Returns list of partially covered lines (sorted ascending) */
	public List<Integer> getPartiallyCoveredLines() {
		return CollectionUtils.sort(partiallyCoveredLines);
	}

	/** Returns list of uncovered lines (sorted ascending) */
	public List<Integer> getUncoveredLines() {
		return CollectionUtils.sort(uncoveredLines);
	}

	/**
	 * Returns the line coverage ratio as a double ([0..1]). This counts
	 * partially covered lines with the factor .5 to the total coverage.
	 */
	public double getCoverageRatio() {
		int lines = getCoverableLines();
		if (lines == 0) {
			return 0;
		}
		return getCoveredLines() / lines;
	}

	/**
	 * Returns the number of lines that are covered, where partially covered
	 * lines are counted half.
	 */
	public double getCoveredLines() {
		return fullyCoveredLines.size() + .5d * partiallyCoveredLines.size();
	}

	/** Returns the number of lines that are coverable. */
	public int getCoverableLines() {
		return fullyCoveredLines.size() + partiallyCoveredLines.size()
				+ uncoveredLines.size();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return String.valueOf(getCoverageRatio());
	}

	/** Returns a string representation of the covered/uncovered lines. */
	public String toLineString() {
		return "Fully covered: "
				+ StringUtils.concat(CollectionUtils.sort(fullyCoveredLines),
						",")
				+ "; partially covered: "
				+ StringUtils.concat(
						CollectionUtils.sort(partiallyCoveredLines), ",")
				+ "; uncovered: "
				+ StringUtils.concat(CollectionUtils.sort(uncoveredLines), ",");
	}

	/**
	 * Replaces the coverable lines with the given lines. This also adjusts the
	 * {@link #fullyCoveredLines} and {@link #partiallyCoveredLines} by removing
	 * all lines that are not coverable.
	 */
	public void setCoverableLines(Set<Integer> lines) {
		fullyCoveredLines.retainAll(lines);
		partiallyCoveredLines.retainAll(lines);

		uncoveredLines.clear();
		uncoveredLines.addAll(lines);
		uncoveredLines.removeAll(fullyCoveredLines);
		uncoveredLines.removeAll(partiallyCoveredLines);
	}
}
