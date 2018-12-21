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
package org.conqat.engine.text.comments.analysis;

import java.util.regex.Pattern;

import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;

/**
 * Base class for comment classifiers based on a feature vector (see also
 * {@link ECommentClassificationFeature}).
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49821 $
 * @ConQAT.Rating GREEN Hash: 59A99E0BC9EC1C3A64B57A555C2CD862
 */
public abstract class CommentClassifierBase implements ICommentClassifier {

	/** Pattern used for identifying task tags. */
	private static final Pattern TASK_TAG_PATTERN = Pattern.compile(
			"\\b(todo|fixme|hack)\\b", Pattern.CASE_INSENSITIVE);

	/** {@inheritDoc} */
	@Override
	public ECommentCategory getClassification(Comment comment) {

		ECommentCategory preclassification = preClassify(comment);
		if (preclassification != null) {
			return preclassification;
		}

		ECommentClassificationFeature[] features = ECommentClassificationFeature
				.values();

		Object[] featureVector = new Object[features.length];
		for (int i = 0; i < features.length; i++) {
			Object value = features[i].extractFromComment(comment);
			if (features[i].isBooleanFeature()) {
				value = boolToDouble((Boolean) value);
			}
			featureVector[i] = value;
		}

		return ECommentCategory.values()[(int) classify(featureVector)];
	}

	/** Template method for performing the machine learning based classification */
	protected abstract double classify(Object[] featureVector);

	/** Converts a value of a boolean feature to a double value */
	private static double boolToDouble(boolean b) {
		if (b) {
			return 1;
		}
		return 0;
	}

	/**
	 * Returns a pre-classification for a comment or null if not
	 * pre-classification was available. The idea is to allow us to fix the
	 * category without the machine learner, if we are able to identify it
	 * up-front and unambiguously. Comments that are detected here, are excluded
	 * from machine learning.
	 */
	public static ECommentCategory preClassify(Comment comment) {
		if (TASK_TAG_PATTERN.matcher(comment.getText()).find()) {
			return ECommentCategory.TASK;
		}

		return null;
	}

}
