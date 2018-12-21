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

package org.conqat.engine.text.comments.utils;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Class to store information about a member (i.e. method or attribute).
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49284 $
 * @ConQAT.Rating GREEN Hash: 0905CA02D0B3E603916192E60B228763
 */
public class Member {

	/** The member's name. */
	private final String memberName;

	/**
	 * Offset of start token of this member. The offset is 0-based and
	 * inclusive.
	 */
	private final int startTokenOffset;

	/**
	 * Start offset of end token of this member. The offset is 0-based and
	 * inclusive.
	 */
	private final int endTokenOffset;

	/** Flag is true if member is a method, false if it is an attribute. */
	private final boolean isMethod;

	/** Constructor */
	public Member(ShallowEntity entity) {
		this.memberName = determineName(entity);
		this.startTokenOffset = entity.includedTokens().get(0).getOffset();
		this.endTokenOffset = CollectionUtils.getLast(entity.includedTokens())
				.getOffset();

		CCSMAssert
				.isTrue(entity.getType() == EShallowEntityType.METHOD
						|| entity.getType() == EShallowEntityType.ATTRIBUTE,
						"Expected either method or attribute, was: "
								+ entity.getType());

		isMethod = entity.getType() == EShallowEntityType.METHOD;
	}

	/**
	 * Returns the name of the entity or (if the name is null) the subtype. An
	 * example for a method without a name is a property get/set method in C#.
	 */
	private static String determineName(ShallowEntity entity) {
		if (!StringUtils.isEmpty(entity.getName())) {
			return entity.getName();
		}
		return entity.getSubtype();
	}

	/** Returns the start offset of the start token. */
	public int getStartTokenOffset() {
		return startTokenOffset;
	}

	/** Returns the start offset of the end token. */
	public int getEndTokenOffset() {
		return endTokenOffset;
	}

	/** Returns the name of this member. */
	public String getMemberName() {
		return memberName;
	}

	/** Returns whether this member is a method. */
	public boolean isMethod() {
		return isMethod;
	}

	/** Returns true if the given offset is contained within this member. */
	public boolean containsOffset(int offset) {
		return offset >= startTokenOffset && offset <= endTokenOffset;
	}
}
