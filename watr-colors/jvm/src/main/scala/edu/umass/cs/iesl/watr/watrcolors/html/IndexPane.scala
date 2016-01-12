package edu.umass.cs.iesl.watr
package watrcolors
package html

// import org.scalajs.jquery.jQuery
// import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}




object IndexPane {


  import scalatags.Text.all._


  def apply()  = {
    <.div(
      <.div()("Query")(
        <.input()
      ),
      <.div()("Explore")(
        "directory cursor"
      )
    )

  }

}

// from index.scala.html
// @(docs: Seq[Document])

// @main("Welcome!") {
//   <h2><img src="@routes.Assets.at("images/iesl-logo.png")"/> Welcome to the IESL SVG annotator tool</h2>
//     <p>The repository has @docs.length SVG file(s):</p>
//     <ul>
//     @for(doc <- docs) {
//       <li><a href="/docs/@doc.fileName">@{ doc.fileName}</a></li>     @* TODO cleaner way to get URI, e.g., http://stackoverflow.com/questions/11133059/play-2-x-how-to-make-an-ajax-request-with-a-common-button *@
//     }
//     </ul>
// }

// From main.scala.html
// @(title: String)(content: Html)

// <!DOCTYPE html>

// <html>
//   <head>
//   <title>@title</title>
//   <link rel="stylesheet" media="screen" href="@routes.Assets.at("stylesheets/main.css")">
//   <link rel="shortcut icon" type="image/png" href="@routes.Assets.at("images/favicon.png")">
//   <script src="@routes.Assets.at("javascripts/jquery-2.1.4.min.js")" type="text/javascript"></script>
//   <script src="@routes.Assets.at("javascripts/fabric.min.js")" type="text/javascript"></script>
//   </head>
//   <body>
//   @content
//   </body>
//   </html>

// // From edit.scala.html
// @(doc : Document)

// @main(s"Editing: $doc.fileName") {
//     <h2 id="loading-header">Initializing...</h2>

//     <hr/>

//     <form onsubmit="return false;">
//         <input id="save-button" type="button" value="Save" title="Save the document's changes.">
//         <input id="revert-button" type="button" value="Revert" title="Discard changes and reload the document.">
//         |
//         <select id="type-select" title="Select the type to use for the next new rectangle.">
//             <option value="title">Title</option>
//             <option value="abstract">Abstract</option>
//             <option value="author">Author</option>
//         </select>
//         <input id="add-rect-button" type="button" value="+R" title="Add a rectangle using the type selected in the dropdown.">
//         <input id="delete-rect-button" type="button" value="-R" title="Remove a rectangle. Select one rect first.">
//         <input id="get-text-button" type="button" value="Text" title="Get text corresponding to a rectangle. Select one rect first.">
//         |
//         <input id="add-link-button" type="button" value="+L" title="Add a link between two rectangles. Select two of the same label that do not have a link between them.">
//         <input id="delete-link-button" type="button" value="-L" title="Remove a link between two rectangles. Select two that have a link between them.">
//         |
//         <div style="font-family: 'Arial'; font-size: 12px; color:red ; display:inline;">1 Title</div>
//         <div style="font-family: 'Arial'; font-size: 12px; color:blue ; display:inline">2 Abstract</div>
//         <div style="font-family: 'Arial'; font-size: 12px; color:green ; display:inline">3 Author</div>
//     </form>

//     <hr/>

//     <div id="overlay-container">
//         <!--<svg:svg id="svg-image" ...></svg:svg>-->   <!-- <svg:svg> element inserted by loadSvg() -->
//         <canvas id="fabric-canvas"></canvas>            <!-- size set by loadSvg() -->
//     </div>


//     <style>
//         #overlay-container {
//         position: relative;
//         }

//         #svg-image {
//         position: absolute;
//         left: 0;
//         top: 0;
//         border: 1px solid black;
//         background-color: ivory;
//         }

//         #fabric-canvas {
//         position: absolute;
//         left: 0;
//         top: 0;
//         }
//     </style>

//     <!-- per http://stackoverflow.com/questions/6672794/playframework-elegant-way-to-pass-values-to-javascript -->
//     <script type="text/javascript">
//         var documentIconURI = "@routes.Assets.at("images/pdf_50x50.png")";
//         var fileRepositoryURI = "@routes.Assets.at("svg-repo")";
//         var fileName= '@doc.fileName';
//     </script>

//     <script src="@routes.Assets.at("javascripts/edit-document.js")" type="text/javascript"></script>

// }
