/*
'variables' passed into this file from edit.scala.html:

o documentIconURI - icon to use at the top of the file
o fileRepositoryURI - where "" is located
o fileName - name of the svg file being edited

 */

$(function() {
    // set up edit controls
    $('#revert-button').click(loadAnnotations);
    $('#save-button').click(saveAnnotations);

    $('#add-rect-button').click(addAnnotation);
    $('#delete-rect-button').click(deleteSelection);
    $('#get-text-button').click(getTextForSelection);

    $('#add-link-button').click(addLineBetweenTwoRects);
    $('#delete-link-button').click(deleteLineBetweenTwoRects);

    // set up shortcut dispatching
    $(document).keydown(handleKeystroke);

    // load the SVG file asynchronously, which sets #svg-image, creates the fabric.Canvas for #fabric-canvas,
    // and loads the initial annotations
    loadSvg();
});

function getColorForLabel(label) {
    var labelToColor = {title: 'red', abstract: 'blue', author: 'green'};
    return labelToColor[label] || 'darkGray';
}

function getFabricCanvas() {
    return $("#fabric-canvas")[0].fabric;
}

function setLoadingHeader(html) {
    var imageHtml = '<img src="' + documentIconURI + '"/> ';
    $("#loading-header").html(imageHtml + html);
}

/**
 * Loads my document's SVG file asynchronously so that we are guaranteed its size is correct. This works around an
 * SVG issue where the document is ready but the <img> element containing the SVG file src was not fully loaded,
 * returning default width and height values (24x24 and 0x0 in Firefox and Chrome respectively). xml loading via
 * http://stackoverflow.com/questions/22822921/how-can-i-get-width-and-height-of-svg-on-image-load-in-ie-11
 * Once loaded, creates the fabric.Canvas for #fabric-canvas and loads the initial annotations.
 *
 * TODO: handle errors - no image or annotations file
 */
function loadSvg() {
    var uri = fileRepositoryURI + '/' + fileName;
    setLoadingHeader("Getting document &OpenCurlyQuote;" + fileName + "&CloseCurlyQuote;...");
    $.get(uri, function(svgxml) {
        // add #svg-image to #overlay-container
        var svgDocEle = document.importNode(svgxml.documentElement, true);
        svgDocEle.setAttribute('id', 'svg-image');
        $('#svg-container').append(svgDocEle);

        // resize #fabric-canvas to overlay SVG
        var canvasEle = document.getElementById("fabric-canvas");
        var attrs = svgxml.documentElement.attributes;
        canvasEle.width = parseInt(attrs.width.value, 10);  // values are like '612px'
        canvasEle.height = parseInt(attrs.height.value, 10);

        // create the Fabric object and save it as an application property for future access
        var canvas = new fabric.Canvas('fabric-canvas');
        canvasEle.fabric = canvas;  // NB: application property: store fabric on canvas element itself - http://stackoverflow.com/questions/13195481/get-the-canvas-object-while-using-fabric-js
        canvas.uniScaleTransform = true;
        fabric.Object.prototype.transparentCorners = false;

        // finally, load the SVG file's annotations
        loadAnnotations();

    }, "xml");
}

function loadAnnotations() {
  // AJAX call to GET annotation JSON
  setLoadingHeader("Loading annotations &OpenCurlyQuote;" + fileName + "&CloseCurlyQuote;...");
  $.get('/annotations/' + fileName,    // TODO cleaner way to get URI
        function(jsonAnnotListStr) {
          var fabricCanvas = getFabricCanvas();
          console.log(jsonAnnotListStr);
          // var jsonAnnotListStrRepl = jsonAnnotListStr.replace(/&quot;/g,'"');     // TODO HACK. Play/Jackson issue?
          // var annotList = JSON.parse(jsonAnnotListStr);
          var annotList = jsonAnnotListStr;
          fabricCanvas.clear();
          for(var annotIdx = 0; annotIdx < annotList.length; annotIdx++) {
            console.log("adding annot");
            var annot = annotList[annotIdx];
            var rects = annot.rects;
            addAnnotationFabricObs(annot.label, rects);
          }
          updateButtonStates([]);
          fabricCanvas.renderAll();
          console.log("rerendering annot");
          setLoadingHeader("Loaded &OpenCurlyQuote;" + fileName + "&CloseCurlyQuote; with "
                           + annotList.length + " annotation(s)");
        }
       );
}

/*
 * Adds Fabric objects corresponding to a single annotation, which is one or more labeled Rects with Lines
 * between each if more than one, i.e., if there are linked rectangles making up the annotation rather than
 * just a single non-linked one.
 */
function addAnnotationFabricObs(label, rects) {
  // add Rects
  var addedRects = [];
  for(var rectIdx = 0; rectIdx < rects.length; rectIdx++) {
    var rect = rects[rectIdx];
    var addedRect = addAnnotationRect(label, rect.x, rect.y, rect.width, rect.height);
    addedRects.push(addedRect);
  }

  // add Lines for each pair of linked Rects
  if ((addedRects.length == 0) || (addedRects.length == 1)) {
    return;
  }

  for(var idx = 0; idx < addedRects.length - 1; idx++) {
    var rect1 = addedRects[idx];
    var rect2 = addedRects[idx + 1];
    addAnnotationLine(rect1, rect2);
  }
}

function addAnnotationRect(label, x, y, width, height) {
  var fabricCanvas = getFabricCanvas();
    var rect = new fabric.Rect({
        left: x,
        top: y,
        width: width,
        height: height,
        fill: null,
        stroke: getColorForLabel(label),
        strokeWidth: 2
    });
    rect.annotLabelType = label;    // NB: application property: store label on Rect object itself
    rect.annotLines = [];           // NB: application property: list of Lines that I participate in - managed by addAnnotationLine()

    rect.setControlVisible('mtr', false);   // disable rotate control
    rect.set({
        cornerSize: 6,
        transparentCorners: true
    });

    fabricCanvas.add(rect);
    fabricCanvas.on({
        'object:selected': handleObjectSelected,
        'selection:created': handleSelectionCreated,
        'selection:cleared': handleSelectionCleared,
        'object:moving': handleObjectMoving,
        'object:scaling': handleObjectScaling,
        'object:modified': handleObjectModified
    });
    return rect;
}

function addAnnotationLine(rect1, rect2) {
    // make Line between rect centers. application properties: a Line has properties for the two Rects that it
    // connects: annotRect1 and annotRect2
    var fabricCanvas = getFabricCanvas();
    var line = new fabric.Line([0, 0, 0, 0], {      // updated by updateLineEndpoints()
        stroke: getColorForLabel(rect1.annotLabelType),
        strokeWidth: 2
    });
    line.set({
        hasBorders: false,
        cornerSize: 6,
        transparentCorners: true,
        selectable: false
    });

    // NB: set application properties
    line.annotRect1 = rect1;
    line.annotRect2 = rect2;
    rect1.annotLines.push(line);
    rect2.annotLines.push(line);
    updateLineEndpoints(line);

    fabricCanvas.add(line);
    fabricCanvas.sendToBack(line);
}

function removeAnnotationLine(line) {   // twin to addAnnotationLine()
    var fabricCanvas = getFabricCanvas();
    var rect1 = line.annotRect1;
    var rect2 = line.annotRect2;

    // clear application properties
    line.annotRect1 = null;
    line.annotRect2 = null;

    var lineIdx = rect1.annotLines.indexOf(line);
    rect1.annotLines.splice(lineIdx, 1);

    lineIdx = rect2.annotLines.indexOf(line);
    rect2.annotLines.splice(lineIdx, 1);

    // remove it
    fabricCanvas.remove(line);
    // fabricCanvas.renderAll();
}


<!-- saveAnnotations() -->

/*
 Creates a JSON data structure that is a list of the annotations corresponding to all the fabric objects. Recall
 that some are Rects and some are Lines between rects, but conceptually these are subsumed by the concept of
 'annotations'. An annotation is a label plus a list of Rects. Also, a single Rect can participate in multiple
 annotations. This means we have to do three passes to group rects into annotation 'objects'. 1) we put each
 Rect into its own annotation (we use an empty {} for 'objects'), then 2) then we iterate over each Line and
 consolidate its two rects into rect1 (arbitrary choice). When we're done, each rect will have an annotation
 'object' associated with it, with annotation objects shared among linked rects. 3) Finally we 'reverse' the
 structure so that we map annotation objects -> all their rects.

 Example: four rects, two links:
 o no links: rect1
 o link1(rect2, rect3)
 o link2(rect2, rect4)

 I.e., graphically:
 r1
 r2 --- r3 (link1)
 \-- r4 (link2)

 Thus we have two annotations: the list [r1] with label r1Label and the list [r2, r3, r4] with label r234Label
 (labels must always be the same). Processing is like this:

 Pass 1: {rect1: annot1, rect2: annot2, rect3: annot3, rect4: annot4}    // where annotN = {}
 Pass 2: {rect1: annot1, rect2: annot2, rect3: annot2, rect4: annot2}
 Pass 3: {annot1: [rect1], annot2: [rect2, rect3, rect4]}

 Finally we serialize this into JSON and PUT it to the server.
 */
function saveAnnotations() {
    // start by collecting separate Rects and Lines lists
    var fabricCanvas = getFabricCanvas();
    var allFabricObjs = fabricCanvas.getObjects();
    var annotRects = [];
    var annotLines = [];
    for(var objIdx = 0; objIdx < allFabricObjs.length; objIdx++) {
        var fabricObj = allFabricObjs[objIdx];
        if(isAnnotatedRect(fabricObj)) {
            annotRects.push(fabricObj);
        } else {
            annotLines.push(fabricObj);
        }
    }

    // pass 1: create separate annotation objects for each rect
    var rectToAnnot = new Map();
    for(var rectIdx = 0; rectIdx < annotRects.length; rectIdx++) {
        var rect = annotRects[rectIdx];
        rectToAnnot.set(rect, {});
    }

    // pass 2: for each Line, merge annotation objects for its two rects
    for(var lineIdx = 0; lineIdx < annotLines.length; lineIdx++) {
        var line = annotLines[lineIdx];
        var rect1 = line.annotRect1;
        var rect2 = line.annotRect2;
        rectToAnnot.set(rect2, rectToAnnot.get(rect1));
    }

    // pass 3: reverse the data structure
    var annotToRects = new Map();
    rectToAnnot.forEach(function(annot, rect, m) {
        if (annotToRects.has(annot)) {
            annotToRects.get(annot).push(rect);
        } else {
            annotToRects.set(annot, [rect]);
        }
    });

    // create the JSON data structure to send
    annotObsToSerialize = [];
    annotToRects.forEach(function(rects, annot, m) {
        // first build rectsListToSerialize, saving the last annotLabelType (arbitrary - they should all be the same)
        var rectsListToSerialize = [];
        var lastAnnotLabelType = null;
        rects.forEach(function (rect) {
            lastAnnotLabelType = rect.annotLabelType;
            rectsListToSerialize.push({'x': rect.left, 'y': rect.top, 'width': rect.width, 'height': rect.height});
        });
        // now add the annotation object itself - a label plus rects list
        var annotObj = {'label': lastAnnotLabelType, 'rects': rectsListToSerialize};
        annotObsToSerialize.push(annotObj);
    });

    // finally, send it
    $.ajax({
        url: '/docs/' + fileName,     // TODO cleaner way to get URI, e.g., http://stackoverflow.com/questions/11133059/play-2-x-how-to-make-an-ajax-request-with-a-common-button
        type: 'PUT',
        contentType: 'application/json',
        data: JSON.stringify(annotObsToSerialize),
        success: function(data) {
            setLoadingHeader("Loaded file &OpenCurlyQuote;" + fileName + "&CloseCurlyQuote; with "
                + annotObsToSerialize.length + " annotation(s)");
        }
    });
}


<!-- selection-related functions -->

// returns true if fabricObj is a rect with a label. importantly, returns false if fabricObj is a group
function isAnnotatedRect(fabricObj){
    return fabricObj.hasOwnProperty('annotLabelType');
}

function handleObjectSelected(e) {
    // check if target is not a group, i.e., if it is a Rect
    var fabricObj = e.target;
    updateButtonStates(isAnnotatedRect(fabricObj) ? [fabricObj] : []);
}

function handleSelectionCreated(e) {
    var fabricGroup = e.target;
    updateButtonStates(fabricGroup.getObjects());
}

function handleSelectionCleared(e) {
    updateButtonStates([]);
}

function updateButtonStates(selectedRects) {
    // get rect and delete rect apply to a single selected Rect
    var isOneSelRect = (selectedRects.length == 1);
    $('#delete-rect-button').prop('disabled', !isOneSelRect);
    $('#get-text-button').prop('disabled', !isOneSelRect);

    // add link applies to two Rects selected, same annotLabelType, no existing link between them
    var isTwoSelRects = (selectedRects.length == 2);
    var isSelRectsSameType = isTwoSelRects && (selectedRects[0].annotLabelType == selectedRects[1].annotLabelType);
    var isLineBetweenRects = isTwoSelRects && (getLineBetweenRects(selectedRects[0], selectedRects[1]) != null);
    var enabled = isTwoSelRects && isSelRectsSameType && !isLineBetweenRects;
    $('#add-link-button').prop('disabled', !enabled);

    // delete link applies to two Rects selected, existing link between them
    enabled = isTwoSelRects && isLineBetweenRects;
    $('#delete-link-button').prop('disabled', !enabled);
}

// return the Line that exists between rect1 and rect2 if there is one, otherwise return null
function getLineBetweenRects(rect1, rect2){
    var r1Lines = rect1.annotLines;
    for (var idx = 0; idx < r1Lines.length; idx++) {
        var line = r1Lines[idx];
        if (((line.annotRect1 == rect1) && (line.annotRect2 == rect2)) ||
            ((line.annotRect1 == rect2) && (line.annotRect2 == rect1))) {
            return line;
        }
    }
    return null;
}


<!-- drag- and resize/scale-related functions -->

// does two things: 1) make stroke width 0 so that scaling doesn't look terrible. 2) redraw all Lines that this
// Rect participates in - similar to http://fabricjs.com/stickman/
function handleObjectMoving(e) {
    var fabricObj = e.target;           // a Rect. TODO: check for type?
    fabricObj.set('strokeWidth', 0);    // for consistent appearance with work-around in handleObjectScaling()
    updateRectEndpoints(fabricObj);
}

function handleObjectScaling(e) {
    var fabricObj = e.target;           // a Rect. TODO: check for type?
    fabricObj.set('strokeWidth', 0);    // a work-around - Fabric shows a thin gray line that does not get scaled
//            updateRectEndpoints(fabricObj);   // TODO: commented out because the line endpoint calculation is off - might have to do with 'ugly' below
}

// since dragging control handes scales instead of resizes, we handle the end of a scaling event by
// setting the actual size based on size and scale factors
// TODO: do this only if scaling and not moving? maybe use originalState to decide
function handleObjectModified(e) {
    var fabricObj = e.target;
    fabricObj.set({'strokeWidth': 2,
        'width': fabricObj.width * fabricObj.scaleX,
        'height': fabricObj.height * fabricObj.scaleY,
        'scaleX': 1,
        'scaleY': 1
    });
    updateRectEndpoints(fabricObj);
}

function updateRectEndpoints(rect) {
    if (!isAnnotatedRect(rect)) {
        return;
    }

    rect.annotLines.forEach(function (line) {
        updateLineEndpoints(line);
    });
}

function updateLineEndpoints(line) {
    // centers line's endpoints on its two rect1 and rect2
    var rect1 = line.annotRect1;
    var rect2 = line.annotRect2;
    var x1 = rect1.left + (rect1.width / 2);
    var y1 = rect1.top + (rect1.height / 2);
    var x2 = rect2.left + (rect2.width / 2);
    var y2 = rect2.top + (rect2.height / 2);
    line.set({'x1': x1, 'y1': y1, 'x2': x2, 'y2': y2});

    var fabricCanvas = getFabricCanvas();
    fabricCanvas.renderAll();
}


<!-- keystroke handling functions -->

function handleKeystroke(e) {
    var handledKey = false;
    switch(e.keyCode) {
        case 8:    // backspace
            handledKey = deleteSelection();
            break;

        case 9:    // tab
            handledKey = selectNextOrPrev(e.shiftKey);
            break;

        case 27:    // escape
            setFilterAnnotationType(null);
            handledKey = true;
            break;

        case 37:    // left
            handledKey = nudgeOrResizeSelection(e.altKey, e.shiftKey ? -10 : -1, 0);
            break;

        case 38:    // up
            handledKey = nudgeOrResizeSelection(e.altKey, 0, e.shiftKey ? -10 : -1);
            break;

        case 39:    // right
            handledKey = nudgeOrResizeSelection(e.altKey, e.shiftKey ? +10 : +1, 0);
            break;

        case 40:    // down
            handledKey = nudgeOrResizeSelection(e.altKey, 0, e.shiftKey ? +10 : +1);
            break;

        case 46:    // delete
            handledKey = deleteSelection();
            break;

        case 49:    // 1
        case 50:    // 2
        case 51:    // 3
            var typeName = {49: 'title', 50: 'abstract', 51: 'author'}[e.keyCode];
            if (e.shiftKey){
                setFilterAnnotationType(typeName);
            } else {
                setNewAnnotationType(typeName);
            }
            handledKey = true;
            break;

        case 61:    // = or +
            if (e.metaKey) {
                break;  // let browser handle, esp. zoom
            }

            addAnnotation();
            handledKey = true;
            break;

        case 68:    // d
            if (e.ctrlKey) {
                duplicateSelection();
                handledKey = true;
            }
            break;

        case 88:    // x
            getTextForSelection();
            handledKey = true;
            break;
    }
    return !handledKey;     // return false -> cancels browser handling it too
}

function deleteSelection() {
    var fabricCanvas = getFabricCanvas();
    var activeObj = fabricCanvas.getActiveObject();     // assume enabled properly: should be one Rect selected
    deleteRectAndLinkedLines(activeObj);
    return true;
}

function deleteRectAndLinkedLines(rect) {
    // remove the Rect itself
    var fabricCanvas = getFabricCanvas();
    fabricCanvas.remove(rect);

    // remove any linked Lines from fabric, and remove from other Rects' annotLines
    var lines = rect.annotLines;
    for (var idx = 0; idx < lines.length; idx++) {
        var line = lines[idx];
        fabricCanvas.remove(line);

        var otherRect = line.annotRect1 == rect ? line.annotRect2 : line.annotRect1;
        var lineIdxOtherRect = otherRect.annotLines.indexOf(line);
        otherRect.annotLines.splice(lineIdxOtherRect, 1);
    }
}

function addLineBetweenTwoRects() {
    var fabricCanvas = getFabricCanvas();
    var selectedRects = fabricCanvas.getActiveGroup().getObjects();     // assume enabled properly: should be two Rects selected, same annotLabelType, no existing link between them
    var rect1 = selectedRects[0];   // arbitrary choice of 1 and 2
    var rect2 = selectedRects[1];

    // a major Fabric gotcha: while in a selection, each rect's coordinates are group/selection-local, not absolute
    // (insane - https://groups.google.com/forum/#!topic/fabricjs/XfFMgo1Da7U ). the workaround: deselect first
    fabricCanvas.deactivateAllWithDispatch();   // clear the selection. MUST be called before addAnnotationLine()
    addAnnotationLine(rect1, rect2);
}

function deleteLineBetweenTwoRects() {
    var fabricCanvas = getFabricCanvas();
    var selectedRects = fabricCanvas.getActiveGroup().getObjects();     // assume enabled properly: should be two Rects selected, existing link between them
    var rect1 = selectedRects[0];   // arbitrary choice of 1 and 2
    var rect2 = selectedRects[1];
    var lineBetween = getLineBetweenRects(selectedRects[0], selectedRects[1]);
    fabricCanvas.deactivateAllWithDispatch();   // clear the selection, for consistency with addLineBetweenTwoRects()
    removeAnnotationLine(lineBetween);
}

function selectNextOrPrev(isPrevious) {
    var fabricCanvas = getFabricCanvas();
    allObjs = fabricCanvas.getObjects();
    var activeObj = fabricCanvas.getActiveObject();     // assume enabled properly: should be one Rect selected
    var newIndex;
    if (!activeObj) {
        newIndex = isPrevious ? allObjs.length - 1 : 0;
    } else {
        activeObjIdx = allObjs.indexOf(activeObj);
        newIndex = activeObjIdx + (isPrevious ? - 1 : 1);   // might go out of bounds
        if (newIndex == -1){
            newIndex = allObjs.length - 1;
        } else if (newIndex == allObjs.length) {
            newIndex = 0;
        }
    }
    objToSelect = allObjs[newIndex];
    fabricCanvas.setActiveObject(objToSelect);
    return true;
}

function setFilterAnnotationType(typeName) {
    // dim all annnotation rects but those of typeName
    var fabricCanvas = getFabricCanvas();
    allObjs = fabricCanvas.getObjects();
    allObjs.forEach(function(obj) {
        if ((obj.annotLabelType == typeName) || !typeName) {
            // un-dim obj
            obj.set('opacity', 1.0)
        } else {
            // dim obj
            obj.set('opacity', 0.2)
        }
    });
    fabricCanvas.renderAll();
}

function setNewAnnotationType(typeName) {
    $('#type-select').val(typeName);
}

function duplicateSelection() {
    var fabricCanvas = getFabricCanvas();
    var activeObj = fabricCanvas.getActiveObject();     // assume enabled properly: should be one Rect selected
    var newRect = addAnnotationRect(activeObj.annotLabelType, activeObj.left + 10, activeObj.top + 10,
        activeObj.width, activeObj.height);
    fabricCanvas.setActiveObject(newRect);
}

// TODO future: let users drag an area on the canvas to create - mouse:down, mouse:move (draw), mouse:up (create).
// for now, add a rect with fixed position and size
function addAnnotation() {
    var fabricCanvas = getFabricCanvas();
    var newAnnotType = $('#type-select').val();
    var newRect = addAnnotationRect(newAnnotType, 10, 10, 100, 25);
    fabricCanvas.setActiveObject(newRect);
    setFilterAnnotationType(null);
}

function getTextForSelection() {
    var fabricCanvas = getFabricCanvas();
    var activeObj = fabricCanvas.getActiveObject();     // assume enabled properly: should be one Rect selected
    if (!activeObj) {
        return;
    }

    var rectData =  {'label': activeObj.annotLabelType,
        'x': activeObj.left, 'y': activeObj.top, 'width': activeObj.width, 'height': activeObj.height};
    $.get('/docs/' + fileName + '/text',       // TODO cleaner way to get URI
        rectData,
        function(data) {
            window.alert('text: ' + data);      // TODO show in reasonable way - overlay on current selection, sidebar, etc.
        }
    );
}

function nudgeOrResizeSelection(isResize, deltaX, deltaY) {
    var fabricCanvas = getFabricCanvas();
    var activeObj = fabricCanvas.getActiveObject();     // assume enabled properly: should be one Rect selected
    if (isResize) {
        activeObj.set({'width': activeObj.width + deltaX,
            'height': activeObj.height + deltaY});
    } else {
        activeObj.set({'left': activeObj.left + deltaX,
            'top': activeObj.top + deltaY});
    }
    activeObj.setCoords();
    updateRectEndpoints(activeObj);
    fabricCanvas.renderAll();
    return true;
}
