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

/** §header§
 * Class for comment findings.
 * @author $Author: hummelb $
 * @version $Rev: 50875 $
 * @ConQAT.Rating RED Hash:
 */
public class CommentFindings {

	
		
		/** inner attribute comment §interface§ */
		private int a1;
		
		/** Inner Constructor §interface§ */
		public CommentFindings () {
			
		}
		
		/** copy §interface§ */
		public void test(){
		}
		
		/** toString §interface§ */
		public String toString(){
			//this should not happen!  §inline§
			return "";
		}
		
		/** Returns number of methods §interface§ */
		public void getNumberOfMethods(){
			//is this right?   §inline§
			return 2;
		}
		
		public void shortComment(){
			//that's useless   §inline§
		}
		
		public void longComment(){
			//this is a very long comment that should contain more than 30 words to create a very long inline comment finding in the test case. aha aha aha aha aha aha aha. hopefully 30 now.  §inline§
		}
		
		public void commentedOutCode(){
//			String result = removeTags(comment);  §code§
//			result = removeCommentIdentifiers(result);
//			result = removeJavaDocElements(result);
//			result = removeLineBreaks(result);
//			result = result.replaceAll("_", " ");
//			result = result.replaceAll("}", "");
//			return result;
		}
			
		/* §interface§
		* (non-Javadoc)
		*
		* @see
		* de.swm.pms.mams.adapter.pm.PmAdapter#getProjectInfoByTechId(java.lang
		* .String)
		*/
		@Override
		public Projekt getProjectInfoByTechId(String id) {
			
		}
}
