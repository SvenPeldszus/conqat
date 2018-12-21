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
package org.conqat.engine.commons.statistics;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.conqat.lib.commons.clone.IDeepCloneable;
import org.conqat.lib.commons.collections.UnmodifiableMap;

/**
 * This class manages multiple named date value series.
 * <p>
 * Overridden methods from DateValueSeries work on the first series only.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49634 $
 * @ConQAT.Rating GREEN Hash: 9A6BF2E4A4286DA2B49F24E4AAE1095A
 */
public class MultiDateValueSeries extends DateValueSeries {

	/** List of series descriptors stored by this object. */
	private final List<SeriesDescriptor> seriesDescriptors = new ArrayList<>();

	/** Create new series. */
	public MultiDateValueSeries() {
		// nothing to do
	}

	/** Copy constructor. */
	private MultiDateValueSeries(MultiDateValueSeries other) {
		for (int i = 0; i < other.seriesDescriptors.size(); ++i) {
			seriesDescriptors.add(other.seriesDescriptors.get(i).deepClone());
		}
	}

	/** Adds a series to this one. */
	public void addSeries(String name, DateValueSeries series, Color color) {
		this.seriesDescriptors.add(new SeriesDescriptor(name, series, color));
	}

	/** Returns the size, i.e. the number of series. */
	public int getSize() {
		return seriesDescriptors.size();
	}

	/** Returns the name of the series with the given index. */
	public String getName(int index) {
		return seriesDescriptors.get(index).name;
	}

	/** Returns the series with the given index. */
	public DateValueSeries getSeries(int index) {
		return seriesDescriptors.get(index).series;
	}

	/** Returns the color with the given index. */
	public Color getColor(int index) {
		return seriesDescriptors.get(index).color;
	}

	/** {@inheritDoc} */
	@Override
	public void addValue(Date date, double value) {
		first().addValue(date, value);
	}

	/** {@inheritDoc} */
	@Override
	public UnmodifiableMap<Date, Double> getValues() {
		return first().getValues();
	}

	/** {@inheritDoc} */
	@Override
	public Date getEarliestDate() {
		return first().getEarliestDate();
	}

	/** {@inheritDoc} */
	@Override
	public Date getLatestDate() {
		return first().getLatestDate();
	}

	/** Returns the first series (or creates one if empty). */
	private DateValueSeries first() {
		if (seriesDescriptors.isEmpty()) {
			seriesDescriptors.add(new SeriesDescriptor("default",
					new DateValueSeries(), Color.black));
		}
		return seriesDescriptors.get(0).series;
	}

	/** {@inheritDoc} */
	@Override
	public MultiDateValueSeries deepClone() {
		return new MultiDateValueSeries(this);
	}

	/** Series descriptor */
	private static class SeriesDescriptor implements IDeepCloneable {
		/** Name of the series. */
		private final String name;

		/** The data series itself. */
		private final DateValueSeries series;

		/** The series color. */
		private final Color color;

		/** Constructor */
		public SeriesDescriptor(String name, DateValueSeries series, Color color) {
			this.name = name;
			this.series = series;
			this.color = color;
		}

		/** Copy constructor */
		private SeriesDescriptor(SeriesDescriptor other) {
			name = other.name;
			series = other.series.deepClone();
			color = other.color;
		}

		/** {@inheritDoc} */
		@Override
		public SeriesDescriptor deepClone() {
			return new SeriesDescriptor(this);
		}
	}

}
