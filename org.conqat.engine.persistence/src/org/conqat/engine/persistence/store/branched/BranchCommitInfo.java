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
package org.conqat.engine.persistence.store.branched;

import java.io.Serializable;

/**
 * Representation of the information for a single commit. Each commit has a
 * symbolic name (represented as bytes to allow arbitrary data to be encoded in
 * it) and links to predecessors.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51574 $
 * @ConQAT.Rating GREEN Hash: 3936B3ADA8723BAB46BDB7D48D1E6363
 */
public class BranchCommitInfo implements Serializable {

	/** Version for serialization. */
	private static final long serialVersionUID = 1;

	/** The symbolic name of this commit. */
	private final byte[] commitName;

	/**
	 * The symbolic name of the direct parent (predecessor) commit. This may be
	 * null for a commit without parent.
	 */
	private final byte[] parentCommitName;

	/**
	 * The symbolic name of the delta predecessor commit. The commit described
	 * by this info will contain the changes of all predecessor up to the delta
	 * predecessor. This may be null if there is no delta predecessor, i.e. this
	 * commit holds all data.
	 */
	private final byte[] deltaPredecessorCommitName;

	/** The depth of the commit, i.e. the number of predecessors. */
	private final int depth;

	/** The current status of this commit. */
	private final ECommitStatus status;

	/** Constructor. */
	/* package */BranchCommitInfo(byte[] commitName,
			byte[] directPredecessorCommitName,
			byte[] deltaPredecessorCommitName, int depth, ECommitStatus status) {
		this.commitName = commitName;
		this.parentCommitName = directPredecessorCommitName;
		this.deltaPredecessorCommitName = deltaPredecessorCommitName;
		this.depth = depth;
		this.status = status;
	}

	/** Copy constructor with new status value. */
	/* package */BranchCommitInfo(BranchCommitInfo other, ECommitStatus status) {
		this(other.commitName, other.parentCommitName,
				other.deltaPredecessorCommitName, other.depth, status);
	}

	/** @see #commitName */
	public byte[] getCommitName() {
		return commitName;
	}

	/** @see #parentCommitName */
	public byte[] getParentCommitName() {
		return parentCommitName;
	}

	/** @see #deltaPredecessorCommitName */
	public byte[] getDeltaPredecessorCommitName() {
		return deltaPredecessorCommitName;
	}

	/** @see #depth */
	public int getDepth() {
		return depth;
	}

	/** @see #status */
	public ECommitStatus getStatus() {
		return status;
	}
}
