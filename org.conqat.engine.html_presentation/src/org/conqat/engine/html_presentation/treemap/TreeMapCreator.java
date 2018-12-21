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
package org.conqat.engine.html_presentation.treemap;

import static org.conqat.engine.commons.css.CSSMananger.DEFAULT_FONT;
import static org.conqat.lib.commons.html.ECSSProperty.HEIGHT;
import static org.conqat.lib.commons.html.ECSSProperty.WIDTH;

import java.awt.Color;
import java.util.regex.Pattern;

import org.conqat.engine.commons.CommonUtils;
import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.html_presentation.image.IImageDescriptor;
import org.conqat.lib.commons.html.CSSDeclarationBlock;
import org.conqat.lib.commons.string.StringUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: goeb $
 * @version $Rev: 51603 $
 * @ConQAT.Rating GREEN Hash: D7423FF77FC9C4EA41E5D5CAACD853DE
 */
@AConQATProcessor(description = "Creates tree map representation of IConQATNode hierarchies.")
public class TreeMapCreator extends ConQATProcessorBase {

	/**
	 * Default value for cushion height. We need this as a string for the
	 * annotation.
	 */
	private static final String DEFAULT_CUSHION_HEIGHT_STRING = "0.5";

	/** Default value for cushion height. */
	public static final double DEFAULT_CUSHION_HEIGHT = Double
			.parseDouble(DEFAULT_CUSHION_HEIGHT_STRING);

	/**
	 * Default value for cushion height. We need this as a string for the
	 * annotation.
	 */
	private static final String DEFAULT_CUSHION_SCALE_STRING = "0.85";

	/** Default value for cushion scale. */
	public static final double DEFAULT_CUSHION_SCALE = Double
			.parseDouble(DEFAULT_CUSHION_SCALE_STRING);

	/** CSS class used for the single colored blocks in the color legend. */
	public static final  CSSDeclarationBlock MAP_LEGEND = new CSSDeclarationBlock(
			DEFAULT_FONT).setBorderStyle("none").setMargin("8px");

	/** Style used for color squares for legends. */
	public static final CSSDeclarationBlock COLOR_SQUARE_STYLE = new CSSDeclarationBlock(
			WIDTH, "30px", HEIGHT, "20px").setBorder("1px", "solid", "black");

	/** Height for the cushions. */
	private double cushionHeight = -1;

	/** Scale of the cushions. */
	private double cushionScale = -1;

	/** Color for node text */
	private Color textColor = null;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.DRAW_LEGEND_PARAM, attribute = ConQATParamDoc.DRAW_LEGEND_ATTRIBUTE, optional = true, description = ""
			+ ConQATParamDoc.DRAW_LEGEND_DESC)
	public boolean drawLegend = true;

	/**
	 * Pattern used to split tree map node names that are too long to be
	 * displayed into parts from which the last part is chosen for display (if
	 * this is null, name is not split)
	 */
	private Pattern separationPattern = null;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.INPUT_NAME, attribute = ConQATParamDoc.INPUT_REF_NAME, description = ""
			+ "Node hierarchy to visualize.")
	public IConQATNode root;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "size", attribute = ConQATParamDoc.READKEY_KEY_NAME, optional = true, description = ""
			+ "Set the key used to retrieve the size of a node. "
			+ "If no key is given, each node will be weighted with 1, i.e. just the number of leaves is counted.")
	public String sizeKey = null;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "color", attribute = ConQATParamDoc.READKEY_KEY_NAME, optional = true, description = ""
			+ "Set the key used to retrieve the color of a node. "
			+ "If no key is given, the value 'color' is used as key. ")
	public String colorKey = "color";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "pattern-color", attribute = ConQATParamDoc.READKEY_KEY_NAME, optional = true, description = ""
			+ "Set the key used to retrieve the color for the pattern of a node. "
			+ "If no key is given, the value 'pattern-color' is used as key. ")
	public String patternColorKey = "pattern-color";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "drawing-pattern", attribute = ConQATParamDoc.READKEY_KEY_NAME, optional = true, description = ""
			+ "Set the key used to retrieve the drawing pattern of a node. "
			+ "If no key is given, the value 'pattern' is used as key. ")
	public String drawingPatternKey = "pattern";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "frames", attribute = "color", optional = true, description = ""
			+ "Determines whether top level elements (i.e. packages, directories) should be made "
			+ "more visible using colored frames. Default is not to draw these frames.")
	public Color topLevelFrameColor = null;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "cushion", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Setup the parameters for cushion layout. Cushion layout adds spot lights to make the "
			+ "hierarchy of the tree more visible. If this parameter is omitted a flat layout will be used.")
	public void setCushions(
			@AConQATAttribute(name = "height", defaultValue = DEFAULT_CUSHION_HEIGHT_STRING, description = ""
					+ "The relative height of the cushions. Valid values are between 0 and 1.") double h,
			@AConQATAttribute(name = "scale", defaultValue = DEFAULT_CUSHION_SCALE_STRING, description = ""
					+ "Scale value for the cushion height. Using a smaller value makes the cushion effect less visible for elements deeper in the tree. Valid values are between 0 and 1.") double f)
			throws ConQATException {

		if (h < 0 || h > 1) {
			throw new ConQATException(
					"Height for cushion must be between 0 and 1!");
		}
		if (f < 0 || f > 1) {
			throw new ConQATException(
					"Scale factor for cushion must be between 0 and 1!");
		}

		cushionHeight = h;
		cushionScale = f;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "text", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Determines whether names of the tree map nodes are displayed. Don't specify a color to switch this off")
	public void setDrawText(
			@AConQATAttribute(name = "color", description = "Color of the text") Color textColor,
			@AConQATAttribute(name = "separation-regexp", description = "Regexp used to split tree map node names that are too long "
					+ "to be displayed into parts from which the last part is chosen for display"
					+ "(if this is null, name is not split). Example: Use '[./]' to split at points and forward slashes. ") String separationRegex)
			throws ConQATException {
		this.textColor = textColor;

		if (StringUtils.isEmpty(separationRegex)) {
			separationPattern = null;
			return;
		}

		separationPattern = CommonUtils.compilePattern(separationRegex);
	}

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "layout", attribute = "squarify", optional = true, description = ""
			+ "Whether to use the squarified layout algorithm. Defaults to false.")
	public boolean useSquarifiedAlgorithm = false;

	/** {@inheritDoc} */
	@Override
	public IImageDescriptor process() {
		return new TreeMapImageDescriptor(cushionHeight, cushionScale,
				topLevelFrameColor, textColor, separationPattern,
				new ColorKeyedTreeMapNode(root, sizeKey, colorKey,
						patternColorKey, drawingPatternKey),
				NodeUtils.getSummary(root), drawLegend, useSquarifiedAlgorithm);
	}

}