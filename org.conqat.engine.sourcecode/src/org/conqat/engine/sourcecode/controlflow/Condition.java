/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: Condition.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.lib.commons.clone.IDeepCloneable;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;

/**
 * The condition of a conditional {@link ControlFlowNode}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: AFBFCF91DE587906BAED6297ED63CA5C
 */
public class Condition implements IDeepCloneable {

	/**
	 * The variables thate are checked for null in this condition.
	 */
	private final Set<String> nullCheckedVariables = new HashSet<String>();

	/**
	 * Information about which variables are always (<code>true</code>) or never
	 * (<code>false</code>) null on the yes branch.
	 */
	private final PairList<String, Boolean> yesBranchInfo = new PairList<String, Boolean>();

	/**
	 * Information about which variables are always (<code>true</code>) or never
	 * (<code>false</code>) null on the no branch.
	 */
	private final PairList<String, Boolean> noBranchInfo = new PairList<String, Boolean>();

	/** Returns nullCheckedVariables. */
	public Set<String> getNullCheckedVariables() {
		return nullCheckedVariables;
	}

	/** Returns yesBranchInfo. */
	public PairList<String, Boolean> getYesBranchInfo() {
		return yesBranchInfo;
	}

	/** Returns noBranchInfo. */
	public PairList<String, Boolean> getNoBranchInfo() {
		return noBranchInfo;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		String representation = "condition(yes=[";
		representation += branchToString(yesBranchInfo);
		representation += "], no=[";
		representation += branchToString(noBranchInfo);
		representation += "], checked="
				+ CollectionUtils.sort(nullCheckedVariables) + ")";
		return representation;
	}

	/** Returns the toString() representation of the given branch info. */
	private String branchToString(PairList<String, Boolean> branchInfo) {
		List<String> representation = new ArrayList<String>();
		for (int i = 0; i < branchInfo.size(); i++) {
			String info = branchInfo.getFirst(i) + "=";
			if (branchInfo.getSecond(i)) {
				info += "A";
			} else {
				info += "N";
			}
			representation.add(info);
		}
		return StringUtils.concat(CollectionUtils.sort(representation), ", ");
	}

	/** {@inheritDoc} */
	@Override
	public Condition deepClone() {
		Condition condition = new Condition();
		condition.getNoBranchInfo().addAll(noBranchInfo);
		condition.getYesBranchInfo().addAll(yesBranchInfo);
		condition.nullCheckedVariables.addAll(nullCheckedVariables);
		return condition;
	}

}
