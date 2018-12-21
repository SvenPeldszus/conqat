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
 * @author $Author: hummelb $
 * @version $Rev: 49625 $
 * @ConQAT.Rating RED Hash:
 */
public class TrivialComments {

	/** Constructor */
	public TrivialComments(){
		
	}
	
	/** blub blub @return*/
	public int get1(){
		return 1;
	}
	
	/**@return*/
	public int get2(){
		return 1;
	}
	
	/**@return the number 1*/
	public int get3(){
		return 1;
	}
	
	/** get that */
	public int getThat(){
		
	}
	
	/**
	 * This method redirect user to edit lounge Access form.
	 * @param modelMap ModelMap to add attribute.
	 * @param request HttpServletRequest
	 * @return String
	 * @throws LTSQLException On error while fetching lounge groups
	 */
	 @RequestMapping(value = "/editLoungeAccessRule.htm", method = RequestMethod.GET)
	 public final String editLoungeAccessRule(){
		 
	 }
	 
	 /** Executes a request and handles the response. */
	@SuppressWarnings("unchecked")
	// TODO (LH) Document that this can return null
	private static <T> T executeRequest(DefaultHttpClient client, HttpRequestBase request, Class<T> resultClass) throws ServiceCallException {
		 
	}
	 
	 // This should not be a finding
}
