package org.conqat.engine.sourcecode.coverage.volume;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50756 $
 * @ConQAT.Rating GREEN Hash: 6B85105546AA019FF7885D7ABF3F8F96
 */
@AConQATProcessor(description = "Reports about coverable methods.")
public class CoverableMethodProcessor extends CoverableEntityProcessorBase {
	
	/** Constructor. */
	public CoverableMethodProcessor() {
		super(EShallowEntityType.METHOD);
	}
}
