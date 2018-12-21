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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.assertion.CCSMPre;

/**
 * Base class for processors that count and list coverable entities of a given
 * type.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51020 $
 * @ConQAT.Rating GREEN Hash: 756462F51C22EED35D661C99CEA7379F
 */
public abstract class CoverableEntityProcessorBase extends
		CoverableVolumeProcessorBase {

	/** The type of entities that are to be counted. */
	private final EShallowEntityType entityType;

	/** Constructor for the given entity type. */
	public CoverableEntityProcessorBase(EShallowEntityType entityType) {
		CCSMPre.isNotNull(entityType);
		this.entityType = entityType;
	}

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		List<ShallowEntity> entities = ShallowParserFactory.parse(element,
				getLogger());
		List<ShallowEntity> matchedEntities = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, entityType);

		List<LineHint> coverageHints = entitiesToString(filterEntities(matchedEntities));
		element.setValue(COVERABLE_VOLUME_KEY, coverageHints.size());
		element.setValue(COVERABLE_HINTS_KEY, coverageHints);
	}

	/**
	 * An additional filter for the matched entities that allows to filter some
	 * of them.
	 */
	protected Collection<ShallowEntity> filterEntities(
			List<ShallowEntity> entities) {
		return entities;
	}

	/** Returns a list of string representations for the given entities. */
	protected List<LineHint> entitiesToString(Collection<ShallowEntity> entities) {
		List<LineHint> entityStrings = new ArrayList<>();
		for (ShallowEntity entity : entities) {
			entityStrings.add(new LineHint(entity.toLocalString(), entity
					.getStartLine()));
		}
		return entityStrings;
	}
}
