import org.conqat.engine.commons.util.SlimmingLogger;

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

/**
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51518 $
 * @ConQAT.Rating RED Hash:
 */
public class UnrelatedCR6584 {
	
	private SlimmingLogger logger;

	/**
	 * Returns a {@link SlimmingLogger}. We overwrite this method as most
	 * programmers are used to access the logger via method
	 * <code>getLogger()</code> and would, hence, be prone to access the
	 * non-slimming logger.
	 */
	@Override
	protected SlimmingLogger getLogger() {
		if (logger == null) {
			logger = new SlimmingLogger(super.getLogger());
		}
		return logger;
	}
	
	/*
	 * Returns an {@link UnmodifiableList} of {@link CustomCheck}s that have
	 * been registered.
	 */
	public UnmodifiableCollection<CustomCheck> getCustomChecks() {
		return CollectionUtils.asUnmodifiable(fileToChecksMap.values());
	}
	
	/**
	 * See
	 * {@link #uniformPathGetCall(EKnownService, Class, String, String, ServerDetails, String...)}
	 * . This implementation has flexible options.
	 */
	static <T> T uniformPathGetCall(EKnownService service,
			Class<T> resultClass, String uniformPath, String projectName,
			ServerDetails serverDetails, Map<String, String> options)
			throws ServiceCallException {
		return null;
	}
	

}
