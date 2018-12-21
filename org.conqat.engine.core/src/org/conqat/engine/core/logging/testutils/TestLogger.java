/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TestLogger.java 51564 2015-01-20 12:51:40Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.core.logging.testutils;

import static org.junit.Assert.assertFalse;

import org.conqat.engine.core.logging.ELogLevel;

/**
 * A {@link LoggerMock} that remembers whether errors were logged.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51564 $
 * @ConQAT.Rating YELLOW Hash: 2EBF7AA66463EC8184332F7B094A864E
 * 
 */
public class TestLogger extends LoggerMock {

	/** Whether at least one error was logged with this logger. */
	private boolean hadError = false;

	/** {@inheritDoc} */
	@Override
	public void log(ELogLevel level, Object message) {
		super.log(level, message);
		if (level == ELogLevel.ERROR || level == ELogLevel.FATAL) {
			hadError = true;
		}
	}

	/** {@inheritDoc} */
	@Override
	public void log(ELogLevel level, Object message, Throwable throwable) {
		super.log(level, message, throwable);
		if (level == ELogLevel.ERROR || level == ELogLevel.FATAL) {
			hadError = true;
		}
	}

	/** {@inheritDoc} */
	@Override
	public void error(Object message) {
		super.error(message);
		hadError = true;
	}

	/** {@inheritDoc} */
	@Override
	public void error(Object message, Throwable throwable) {
		super.error(message, throwable);
		hadError = true;
	}

	/** Raises an exception if an error was logged to this logger. */
	public void assertNoErrorsOccurred() {
		assertFalse(
				"The analysis logged at least one error during the test run.",
				hadError);
	}

}
