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
package org.conqat.engine.commons.findings;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.conqat.engine.commons.findings.location.ElementLocation;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.clone.IDeepCloneable;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableMap;

/**
 * This class describes orphaned finding that is attached to a node or a
 * findings report. This is useful if e.g. findings have been filtered but
 * certain operations should still be carried out on the findings.
 * 
 * This is not implemented as subclass of {@link Finding} as code dealing with
 * {@link DetachedFinding}s is expected to work quite differently than code
 * dealing with normal {@link Finding}s.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47624 $
 * @ConQAT.Rating GREEN Hash: 0280CBF29A060C604B2E3E735287108B
 */
public class DetachedFinding implements IDeepCloneable, Serializable {

	/** Serial version UID. */
	private static final long serialVersionUID = 1;

	/** The location. */
	private ElementLocation location;

	/** The group name. */
	private String groupName;

	/** The category name. */
	private String categoryName;

	/** The message. */
	private final String message;

	/** The assessment color of the finding (may be null). */
	private ETrafficLightColor assessment;

	/**
	 * The locations of other findings that are considered siblings. This is,
	 * e.g., used to find other clone instances in the same clone class. As this
	 * is commonly empty, we keep this attribute null in this case to save space
	 * for serialization. The access methods, however, handle this
	 * transparently.
	 */
	private List<ElementLocation> siblingLocations;

	/**
	 * Numeric properties for this finding. Each finding can be associated with
	 * one or more properties describing details of the finding (length of long
	 * method, etc.). These properties are also displayed in the UI and can be
	 * used for sorting findings.
	 */
	private final Map<String, Double> properties = new HashMap<String, Double>();

	/** Constructor. */
	public DetachedFinding(String groupName, String categoryName,
			String message, ElementLocation location) {
		this(groupName, categoryName, message, location, null);
	}

	/** Constructor. The assessment may be <code>null</code> */
	public DetachedFinding(String groupName, String categoryName,
			String message, ElementLocation location,
			ETrafficLightColor assessment) {
		CCSMAssert.isNotNull(location);

		this.groupName = groupName;
		this.categoryName = categoryName;
		this.message = message;
		this.location = location;
		this.assessment = assessment;
	}

	/**
	 * Constructor. No reference to the finding is kept. However, the locations
	 * are added to the detached finding.
	 */
	public DetachedFinding(Finding finding) {
		this(finding.getParent().getName(), finding.getParent().getParent()
				.getName(), finding.getMessage(), finding.getLocation(),
				(ETrafficLightColor) finding.getValue(EFindingKeys.ASSESSMENT
						.toString()));
		properties.putAll(finding.getProperties());
	}

	/** Copy constructor. */
	protected DetachedFinding(DetachedFinding other) {
		this(other.groupName, other.categoryName, other.message,
				other.location, other.assessment);

		if (other.hasSiblings()) {
			this.siblingLocations = new ArrayList<ElementLocation>(
					other.siblingLocations);
		}

		properties.putAll(other.properties);
	}

	/** Get group name. */
	public String getGroupName() {
		return groupName;
	}

	/** Sets group name. */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	/** Get category name. */
	public String getCategoryName() {
		return categoryName;
	}

	/** Sets category name. */
	public void setCategoryName(String categoryName) {
		this.categoryName = categoryName;
	}

	/** Get location. */
	public ElementLocation getLocation() {
		return location;
	}

	/** Sets location. */
	public void setLocation(ElementLocation location) {
		CCSMPre.isNotNull(location);
		this.location = location;
	}

	/** Get location string. */
	public String getLocationString() {
		return location.toLocationString();
	}

	/** Get message. */
	public String getMessage() {
		return message;
	}

	/** Returns whether this findings has siblings. */
	public boolean hasSiblings() {
		return siblingLocations != null && !siblingLocations.isEmpty();
	}

	/** Returns the locations of sibling findings. */
	public List<ElementLocation> getSiblingLocations() {
		if (siblingLocations == null) {
			return CollectionUtils.emptyList();
		}
		return CollectionUtils.asUnmodifiable(siblingLocations);
	}

	/** Adds locations of sibling findings. */
	public void addSiblingLocation(ElementLocation location) {
		if (siblingLocations == null) {
			siblingLocations = new ArrayList<ElementLocation>();
		}
		siblingLocations.add(location);
	}

	/** Returns the properties of this finding. */
	public UnmodifiableMap<String, Double> getProperties() {
		return CollectionUtils.asUnmodifiable(properties);
	}

	/** Sets a property for this finding. */
	public void setProperty(String name, double value) {
		properties.put(name, value);
	}

	/** Returns assessment. */
	public ETrafficLightColor getAssessment() {
		return assessment;
	}

	/** Sets the assessment. */
	public void setAssessment(ETrafficLightColor assessment) {
		this.assessment = assessment;
	}

	/**
	 * Returns a string representation that contains the, message, the group and
	 * and location hint.
	 */
	@Override
	public String toString() {
		return getMessage() + " (" + getGroupName() + ") @ "
				+ location.toLocationString();
	}

	/** {@inheritDoc} */
	@Override
	public DetachedFinding deepClone() {
		return new DetachedFinding(this);
	}
}
