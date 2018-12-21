// collection of true and false positives for unrelated comments. This file would not compile...

public class Unrelated {
	
	/** This should be unrelated. */
	public int foo;
	
	/**
	 * Prefix used with basic authentication
	 * (http://en.wikipedia.org/wiki/Basic_access_authentication) including the
	 * trailing space.
	 */
	public static final String BASIC_AUTHENTICATION_PREFIX = "Basic ";
	
	/** Service call to retrieve content for container info. */
	public ContainerInfo getContainerInfo(String uniformPath,
			String projectName, ServerDetails serverDetails)
			throws ServiceCallException {
		return uniformPathGetCall("dir", ContainerInfo.class, uniformPath,
				projectName, serverDetails);
	}
	
}
