goog.provide('ts.architecture.editor.ComponentDragAndClickHandler');

goog.require('ts.architecture.editor.ComponentHandlerBase');

/**
 * @author $Author: hummelb $
 * @version $Rev: 50156 $
 * @ConQAT.Rating YELLOW Hash: 953311C5EC15E17EB0F2E49653E6FC97
 * 
 * @constructor
 * @extends {ts.architecture.editor.ComponentHandlerBase}
 * 
 * Class handling the dragging and the clicking on a component.
 * 
 * @param {ts.architecture.editor.ComponentSvgHandler} svgHandler svgHandler of
 *            the component
 */
ts.architecture.editor.ComponentDragAndClickHandler = function(svgHandler) {
	goog.base(this, svgHandler);

	/**
	 * Flag, which helps handling differentiating between a click and a drag.
	 * 
	 * @type {boolean}
	 */
	this.clickedOn = false;

	/**
	 * Number of ms since the last click.
	 * 
	 * @type {number}
	 */
	this.lastClick = 0;

	/**
	 * Threshold value for the maximum number of ms between to clicks to be
	 * registered as a dbl-click
	 */
	this.dblClickThreshold = 500;
};
goog.inherits(ts.architecture.editor.ComponentDragAndClickHandler,
		ts.architecture.editor.ComponentHandlerBase);

/**
 * Makes the component draggable.
 * 
 * @param {boolean} set activated, if true.
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.set = function(
		set) {
	if (set) {
		this.svgHandler.svg.undrag(); // in case drag is already set
		this.svgHandler.svg.drag(this.mouseMove, this.mouseDown, this.mouseUp,
				this, this, this);
	} else {
		this.svgHandler.svg.undrag();
	}
};

/**
 * Start for the component drag event.
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.mouseDown = function() {
	this.svgHandler.toFront();
	this.previousX = 0;
	this.previousY = 0;

	this.dragRect = this.editor.paper.rect(this.svgHandler.svg.attr('x'),
			this.svgHandler.svg.attr('y'), this.svgHandler.svg.attr('width'),
			this.svgHandler.svg.attr('height'), 8);

	this.clickedOn = true;

	this.editor.setActiveElement(this.component);
};

/**
 * Move for the component drag-event.
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.mouseMove = function(
		dx, dy, x, y) {
	this.clickedOn = false;
	var relativeX = x - this.editor.paperPosition[0];
	var relativeY = y - this.editor.paperPosition[1];
	var txGroup = dx - this.previousX;
	var tyGroup = dy - this.previousY;
	var rect = this.dragRect;

	if (this.editor.paper.width - rect.attr('width') < rect.attr('x') + txGroup
			|| relativeX > this.editor.paper.width) {
		txGroup = this.editor.paper.width - rect.attr('width') - rect.attr('x');
	} else if (rect.attr('x') + txGroup < 0 || relativeX < 0) {
		txGroup = -rect.attr('x');
	}

	if (this.editor.paper.height - rect.attr('height') < rect.attr('y')
			+ tyGroup
			|| relativeY > this.editor.paper.height) {
		tyGroup = this.editor.paper.height - rect.attr('height')
				- rect.attr('y');
	} else if (rect.attr('y') + tyGroup < 0 || relativeY < 0) {
		tyGroup = -rect.attr('y');
	}

	this.dragRect.move(txGroup, tyGroup);

	var intersectedComponents = this.editor.components
			.getIntersectedComponents(this.dragRect.getBBox(), this.component);
	if (intersectedComponents.length != 0) {
		this.dragRect.attr({
			'stroke' : 'red'
		});
	} else {
		this.dragRect.attr({
			'stroke' : 'black'
		});
	}

	this.previousX = dx;
	this.previousY = dy;
};

/**
 * Mouse up event handler
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.mouseUp = function(
		event) {
	if (this.clickedOn) {
		if (event.timeStamp - this.lastClick <= this.dblClickThreshold) {
			this.dblclick(event);
			this.lastClick = 0;
		} else {
			this.lastClick = event.timeStamp;
		}

	}

	this.dragEnd();
};

/**
 * End for the component drag-event.
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.dragEnd = function() {
	var dx = this.dragRect.attr('x') - this.svgHandler.svg.attr('x');
	var dy = this.dragRect.attr('y') - this.svgHandler.svg.attr('y');

	var droppedOn = this.editor.components.getDroppedOnComponent(this.dragRect
			.getBBox(), this.component);

	if (droppedOn.move) {
		if (droppedOn.comp) {
			if (this.component.parent) {
				if (this.component.parent != droppedOn.comp) {
					this.component.parent.removeSubComponent(this.component);
					droppedOn.comp.addSubComponent(this.component);
				}
			} else {
				droppedOn.comp.addSubComponent(this.component);
			}
		} else if (this.component.parent) {
			this.component.parent.removeSubComponent(this.component);
		}

		this.svgHandler.move(dx, dy);
	}

	this.dragRect.remove();
	this.svgHandler.svg.getBBox(); // fix for a strange glow-bug
	this.svgHandler.setGlow(true);
};

/**
 * Handler for the click-event on a component.
 */
ts.architecture.editor.ComponentDragAndClickHandler.prototype.dblclick = function(
		event) {
	var position = this.editor.getPositionInPaper(event);
	var textElement = this.svgHandler.svg.group[1];
	if (textElement.isPointInside(position[0], position[1])) {
		this.svgHandler.triggerNameChanging();
	}
};
