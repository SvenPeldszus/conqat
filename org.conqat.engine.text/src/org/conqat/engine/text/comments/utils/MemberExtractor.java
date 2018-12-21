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

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.IShallowEntityVisitor;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.IToken;

/**
 * The member extractor uses shallow parser for method and field extraction.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47867 $
 * @ConQAT.Rating GREEN Hash: 55CE769BA2191377A49CCEA09352AD82
 */
public class MemberExtractor {

	/** The underlying token element */
	private final ITokenElement element;

	/** The underlying tokens */
	private final List<IToken> tokens;

	/** The extracted members, i.e. methods and fiels. */
	private List<Member> members = new ArrayList<Member>();

	/**
	 * Constructor which also starts the extraction of methods and fields.
	 */
	public MemberExtractor(ITokenElement element, List<IToken> tokens)
			throws ConQATException {
		this.element = element;
		this.tokens = tokens;
		extractMembers();
	}

	/** Parsing call to extract all methods and fields. */
	private void extractMembers() throws ConQATException {
		List<ShallowEntity> entities = ShallowParserFactory.createParser(
				element.getLanguage()).parseTopLevel(tokens);
		ShallowEntity.traverse(entities, new MemberVisitor());
	}

	/**
	 * Returns true if the given name is a method name and the position of the
	 * name is within the methods token.
	 */
	public boolean containsMemberIdentifier(String memberName, int positionName) {
		for (Member member : members) {
			if (member.getMemberName().equals(memberName)
					&& member.containsOffset(positionName)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns true if the given position of a comment token is within a method.
	 */
	public boolean isInsideMethod(int commentPosition) {
		IToken comment = tokens.get(commentPosition);
		for (Member member : members) {
			if (member.isMethod() && member.containsOffset(comment.getOffset())) {
				return true;
			}
		}
		return false;
	}

	/** Visitor for extracting all members and storing it into the given list. */
	private final class MemberVisitor implements IShallowEntityVisitor {

		/** {@inheritDoc} */
		@Override
		public boolean visit(ShallowEntity entity) {
			if (entity.getType() == EShallowEntityType.ATTRIBUTE
					|| entity.getType() == EShallowEntityType.METHOD) {
				members.add(new Member(entity));
				return false;
			}
			return true;
		}

		/** {@inheritDoc} */
		@Override
		public void endVisit(ShallowEntity entity) {
			// not needed
		}
	}
}
