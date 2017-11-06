/**
   Credit to : http://bl.ocks.org/paradite/71869a0f30592ade5246
   **/
function initD3DragSelect(callback) {
    var selectionRect = {
        element			: null,
        previousElement : null,
        currentY		: 0,
        currentX		: 0,
        originX			: 0,
        originY			: 0,
        setElement: function(ele) {
            this.previousElement = this.element;
            this.element = ele;
        },
        getNewAttributes: function() {
            var x = this.currentX<this.originX?this.currentX:this.originX;
            var y = this.currentY<this.originY?this.currentY:this.originY;
            var width = Math.abs(this.currentX - this.originX);
            var height = Math.abs(this.currentY - this.originY);
            return {
                x       : x,
                y       : y,
                width  	: width,
                height  : height
            };
        },
        getCurrentAttributes: function() {
            // use plus sign to convert string into number
            var x = +this.element.attr("x");
            var y = +this.element.attr("y");
            var width = +this.element.attr("width");
            var height = +this.element.attr("height");
            return {
                x1  : x,
                y1	: y,
                x2  : x + width,
                y2  : y + height
            };
        },
        getCurrentAttributesAsText: function() {
            var attrs = this.getCurrentAttributes();
            return "x1: " + attrs.x1 + " x2: " + attrs.x2 + " y1: " + attrs.y1 + " y2: " + attrs.y2;
        },
        init: function(newX, newY) {
            var rectElement = svg.append("rect")
                .attr({
                    rx      : 4,
                    ry      : 4,
                    x       : 0,
                    y       : 0,
                    width   : 0,
                    height  : 0
                })
                .classed("selection", true);
            this.setElement(rectElement);
            this.originX = newX;
            this.originY = newY;
            this.update(newX, newY);
        },
        update: function(newX, newY) {
            this.currentX = newX;
            this.currentY = newY;
            this.element.attr(this.getNewAttributes());
        },
        focus: function() {
            this.element
                .style("stroke", "#DE695B")
                .style("stroke-width", "2.5");
        },
        remove: function() {
            this.element.remove();
            this.element = null;
        },
        removePrevious: function() {
            if(this.previousElement) {
                this.previousElement.remove();
            }
        }
    };


    var svg = d3.select("svg");

    function dragStart() {
        // console.log("dragStart");
        var mouseEvent = d3.event.sourceEvent;
        // console.log("mouse event:");
        // console.dir(mouseEvent);

        // 0=left, 1=middle, 2=right
        var b = mouseEvent.button;
        if (b==0) {
            var p = d3.mouse(this);
            selectionRect.init(p[0], p[1]);
            selectionRect.removePrevious();
            d3.event.sourceEvent.stopPropagation(); // silence other listeners
        }
    }

    function dragMove() {
        if (selectionRect.element != null) {
            // console.log("dragMove");
            var p = d3.mouse(this);
            selectionRect.update(p[0], p[1]);
            var currAttrs = selectionRect.getCurrentAttributes();
            callback({
                move : currAttrs
            });
        }
    }

    function dragEnd() {
        if (selectionRect.element != null) {
            // console.log("dragEnd");
            var finalAttributes = selectionRect.getCurrentAttributes();
            // console.dir(finalAttributes);
            if(finalAttributes.x2 - finalAttributes.x1 > 1 && finalAttributes.y2 - finalAttributes.y1 > 1){
                // console.log("range selected");
                d3.event.sourceEvent.preventDefault();
                // selectionRect.focus();
                selectionRect.remove();
                callback({
                    rect : finalAttributes
                });
            } else {
                // console.log("single point");
                selectionRect.remove();
                callback({
                    point : {
                        x: finalAttributes.x1,
                        y: finalAttributes.y1
                    }
                });
            }
        }
    }

    var dragBehavior = d3.behavior.drag()
        .on("drag", dragMove)
        .on("dragstart", dragStart)
        .on("dragend", dragEnd);

    svg.call(dragBehavior);

}
