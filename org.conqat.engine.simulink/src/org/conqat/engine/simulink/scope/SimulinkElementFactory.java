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
package org.conqat.engine.simulink.scope;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.text.TextElementFactory;
import org.conqat.lib.simulink.builder.ModelBuildingParameters;
import org.conqat.lib.simulink.builder.SimulinkModelBuilder;
import org.conqat.lib.simulink.builder.SimulinkModelBuildingException;
import org.conqat.lib.simulink.model.SimulinkModel;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50675 $
 * @ConQAT.Rating GREEN Hash: 20519BCFC43F8543D2EABA9E24341E58
 */
@AConQATProcessor(description = "Factory for token elements.")
public class SimulinkElementFactory extends TextElementFactory {

	/**
	 * {@inheritDoc}
	 * 
	 * @throws ConQATException
	 *             if the model could not be built
	 */
	@Override
	public ISimulinkElement create(IContentAccessor accessor)
			throws ConQATException {
		try {
			return new SimulinkElement(accessor, encoding, buildModel(accessor));
		} catch (SimulinkModelBuildingException | IOException ex) {
			throw new ConQATException("Could not build model for "
					+ accessor.getLocation() + ": " + ex.getMessage(), ex);
		}
	}

	/** Build Simulink model. */
	private SimulinkModel buildModel(IContentAccessor accessor)
			throws ConQATException, SimulinkModelBuildingException, IOException {
		SimulinkModelBuilder modelBuilder = new SimulinkModelBuilder(
				new ByteArrayInputStream(accessor.getContent()), getLogger(),
				accessor.getUniformPath(), accessor.getUniformPath());
		return modelBuilder.buildModel(new ModelBuildingParameters()
				.setCharset(encoding));
	}
}