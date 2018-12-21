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
package org.conqat.engine.text.comments.analysis;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for processors that classify each comment.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49701 $
 * @ConQAT.Rating GREEN Hash: CF201E4D3D72024438358FBA64C3D6C1
 */
public abstract class CommentClassificationAnalysisBase extends
		CommentAnalysisBase {

	/**
	 * Available classifiers, maps from language to corresponding classifier.
	 */
	private static final Map<ELanguage, ICommentClassifier> CLASSIFIERS = new HashMap<ELanguage, ICommentClassifier>();

	static {
		CLASSIFIERS.put(ELanguage.JAVA, new JavaCommentClassifier());
		CLASSIFIERS.put(ELanguage.CPP, new CppCommentClassifier());
		CLASSIFIERS.put(ELanguage.CS, new CsCommentClassifier());
	}

	/**
	 * Returns whether the given language is supported for comment
	 * classification.
	 */
	public static boolean isSupportedLanguage(ELanguage language) {
		return CLASSIFIERS.containsKey(language);
	}

	/** Returns the comment classifier for the given language (or null). */
	public static ICommentClassifier getCommentClassifier(ELanguage language) {
		return CLASSIFIERS.get(language);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComments(List<Comment> comments,
			ITokenElement element, List<IToken> tokens) throws ConQATException {
		ELanguage language = element.getLanguage();
		if (!isSupportedLanguage(language)) {
			getLogger().warn(
					"Unsupported language for comment classification: "
							+ language);
			return;
		}

		for (Comment comment : comments) {
			analyzeComment(element, comment, getCommentClassifier(language)
					.getClassification(comment));
		}
	}

	/**
	 * This method needs to be implemented by any subclass to analyze the given
	 * comment depending on its category.
	 */
	protected abstract void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) throws ConQATException;

}