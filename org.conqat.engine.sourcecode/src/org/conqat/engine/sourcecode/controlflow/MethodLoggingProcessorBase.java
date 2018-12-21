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
package org.conqat.engine.sourcecode.controlflow;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.ConQATLoggerBase;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.resource.analysis.ElementAnalyzerBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Base class for processors that work with methods, which wraps the logger to
 * provide additional information about the currently analyzed method.
 * 
 * Subclasses can control this behaviour with {@link #startAnalyzing(String)}
 * and {@link #finishedAnalyzing()}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51777 $
 * @ConQAT.Rating YELLOW Hash: 41922581AC5F0BBF8E1606528A7B9751
 */
public abstract class MethodLoggingProcessorBase extends
		ElementAnalyzerBase<ITokenResource, ITokenElement> {

	/**
	 * The currently analyzed element or <code>null</code> if not currently
	 * analyzing anything.
	 */
	private ITokenElement element = null;

	/**
	 * The name of the currently analyzed method or <code>null</code> if not
	 * currently analyzing anything.
	 */
	private String methodName = null;

	/**
	 * Should be called by subclasses to indicate that the given form is now
	 * being processed. The form's name will then be appended to all log
	 * messages.
	 */
	protected void startAnalyzing(String name) {
		this.methodName = name;
	}

	/**
	 * Should be called by subclasses to indicate that processing of methods has
	 * stopped. Log messages will return to normal.
	 */
	protected void finishedAnalyzing() {
		this.methodName = null;
	}

	/** {@inheritDoc} */
	@Override
	protected IConQATLogger getLogger() {
		return new Logger(super.getLogger());
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeElement(ITokenElement element) throws ConQATException {
		this.element = element;
		try {
			analyzeMethodElement(element);
		} catch (Exception | Error e) {
			if (methodName == null) {
				throw e;
			}
			throw new ConQATException("Error while analyzing file "
					+ element.getUniformPath() + ", method " + methodName, e);
		}
	}

	/** Called once for each analyzed element. */
	protected abstract void analyzeMethodElement(ITokenElement element)
			throws ConQATException;

	/**
	 * Wraps an {@link IConQATLogger} and appends information about the
	 * currently analyzed element and method to the log message.
	 */
	private class Logger extends ConQATLoggerBase {

		/** The wrapped logger. */
		private IConQATLogger logger;

		/** Constructor. */
		public Logger(IConQATLogger logger) {
			this.logger = logger;
		}

		/** {@inheritDoc} */
		@Override
		public void log(ELogLevel level, Object message) {
			logger.log(level, message.toString() + getLocationInfo());
		}

		/**
		 * Returns the location information for the currently processed element
		 * and method.
		 */
		private String getLocationInfo() {
			if (methodName == null) {
				return StringUtils.EMPTY_STRING;
			}

			return " - in element " + element.getUniformPath() + ", method "
					+ methodName;
		}

		/** {@inheritDoc} */
		@Override
		public void log(ELogLevel level, Object message, Throwable throwable) {
			logger.log(level, message + getLocationInfo(), throwable);
		}

		/** {@inheritDoc} */
		@Override
		public ELogLevel getMinLogLevel() {
			return logger.getMinLogLevel();
		}

	}

}
