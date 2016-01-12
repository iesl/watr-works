# paper-header-annotator-2
A prototype web application for labeling/annotating PDF files stored as SVG documents (see
[iesl-pdf-to-text](https://github.com/iesl/iesl-pdf-to-text) for how they're generated). It has a bare-bones UI and
layout (i.e., no Bootstrap or equivalent) and no production-level features such as authentication, authorization,
document owners and assignments, error handling, concurrent access, deployment, etc. There is one unit test -
SvgFileDAOTest. Also missing is a comprehensive set of guidelines that include marked up examples that cover all
possibly confusing cases.


# Running
Download the [Typesafe Activator](https://www.typesafe.com/get-started) and run:
```
$ cd <repos>/playframework_temp/
$ <activator_location>/activator-dist-1.3.5/activator run
```
It should compile the necessary files and then start a local server at [localhost:9000](http://localhost:9000).


# Dependencies
* Front end: [Fabric.js Javascript Canvas Library](http://fabricjs.com/) for the rectangles.
* Back end: [Play Framework](https://playframework.com/) for the web application, using the Java API.
* Eventually: [xml_annotator](https://github.com/iesl/xml-annotator) for controllers.Application.getDocRectText()


# Status
## Front end
Currently implemented are two user Play routes: an index at '/' that shows all SVG files in the SvgFileDAO.svgDirectory,
and an SVG editor of individual files at 'docs/<svg_filename>'.

Editor operations currently supported:

* load and display saved annotations using color-coded rectangles
* create/select/edit/delete/filter rectangles using mouse or shortcut keys

TODO:

* abstract out annotation types and colors in edit.scala.html (currently hardcoded). see code __TODO__'s as well
* add annotation types to all desired header fields: abstract (including "abstract" section header), __address__,
  __author__ (including first__, __middle__, __last), __date__, __editor__, __email__, __institution__, __keyword__
  section (whole section including "keywords" section header), __note__, __tech__, __title__.
    * Q: publication info?
    * Q: __web__?
    * Q: is the entire header itself to be marked?
* add shortcut key for addLinkBetweenTwoRects() and deleteLinkBetweenTwoRects()
* shortcut keys should be enabled/disabled same as buttons - see updateButtonStates()
* style the interface using [Bootstrap](http://getbootstrap.com/) or similar
* add document paging to the index 


## Back end
Currently using a filesystem-based scheme for storing the SVG files and their corresponding JSON files in the same
directory. The program creates empty JSON files for any missing ones when starting up.  

TODO:

* controllers.Application.getDocRectText(). Requires modifying [iesl-pdf-to-text](https://github.com/iesl/iesl-pdf-to-text)
  to expose font size information so that each char's bounding box can be calculated. Currently there's a common bug
  where font-size="1px".


## SVG rendering issues
There are two known issues where the SVG files do not show correctly in browsers:

1. Some files have a black background, such as 0158.pdf.svg in the MIT corpus.
2. In Firefox 39.0, some files have overlapping text, such as 4789.pdf.svg in the MIT corpus. Chrome 44 renders correctly.


# Documentation
The UI is a straightforward direct manipulation one where users work with rectangle objects. Click to select, drag
to move, drag resize handles to resize, click the delete rectangle button to remove, etc. The only feature that's non-
obvious is how to add and remove links between rectangles. To add a link, select exactly two rectangles with the same
label and no existing link and then click the add link button. To remove a link, select two rectangles with an existing
link and the click the remove link button.

## Keystroke shortcuts
### Types

<pre>
          1: set current type to Title
 shift  + 1: filter all but Title
          2: set current type to Abstract
 shift  + 2: filter all but Abstract
          3: set current type to Author
 shift  + 3: filter all but Author
     Escape: reset filter
</pre>

### Create

<pre>
+: create new annotation using current type
</pre>

### Move & Resize

<pre>
                     arrow key: move selection 1px
            shift  + arrow key: move selection 10px
           option  + arrow key: resize selection 1px
 shift  +  option  + arrow key: resize selection 10px
</pre>

### Delete/Duplicate

<pre>
backspace|delete: delete selection
    control  + d: duplicate selection
</pre>

### Select

<pre>
          tab: select next
 shift  + tab: select previous
</pre>

### Text Feedback

<pre>
x: display text for selection
</pre>
