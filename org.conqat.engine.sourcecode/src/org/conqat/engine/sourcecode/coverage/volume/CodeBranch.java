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
package org.conqat.engine.sourcecode.coverage.volume;

import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMPre;

/**
 * A code branch that is introduced by a specific entity in the code.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51020 $
 * @ConQAT.Rating GREEN Hash: 82CADBB5888BDA1623652431958CDA58
 */
public class CodeBranch {

	/** The code branch entity. */
	private ShallowEntity entity;

	/**
	 * The decision leading to this branch, null if there is none (e.g. case
	 * statements).
	 */
	private String decision;

	/** Creates a new non-conditional {@link CodeBranch} with the given entity. */
	public CodeBranch(ShallowEntity entity) {
		this(entity, null);
	}

	/** Creates a new {@link CodeBranch} with the given entity. */
	public CodeBranch(ShallowEntity entity, boolean conditional) {
		this(entity, Boolean.toString(conditional));
	}

	/** Creates a new {@link CodeBranch} with the given entity. */
	public CodeBranch(ShallowEntity entity, String decision) {
		CCSMPre.isNotNull(entity);
		this.entity = entity;
		this.decision = decision;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(entity.getSubtype());
		builder.append("(");
		builder.append(entity.getStartLine());
		builder.append("-");
		builder.append(entity.getEndLine());
		builder.append(")");
		if (decision != null) {
			builder.append(" - ");
			builder.append(decision);
		}
		return builder.toString();
	}

	/** Returns the line number for this branch. */
	public int getLine() {
		return entity.getStartLine();
	}
}
