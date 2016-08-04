package edu.umass.cs.iesl.watr
package watrcolors
package server

import net.sf.jsi.Rectangle

import ammonite.ops._

import GeometricFigure._

class SvgOverviewServer(
  rootDirectory: Path
) extends SvgOverviewApi  {
  lazy val corpus = Corpus(rootDirectory)

  def getLabelOverlay(entryDescriptor: String): List[TraceLog] = {
    println(s"getting corpusEntry '${entryDescriptor}'")

    val conf = extract.AppConfig(
      corpusRoot = rootDirectory.toIO.some,
      inputEntryDescriptor = entryDescriptor.some,
      action = "docseg".some,
      force = true
    )

    val traces = extract.Works.segmentDocument(conf)
    val traceLog = traces.head.map(TypeConverters.convertVisualTraceTypes(_))

    traceLog.toList
  }

  def createView(corpusEntryId: String): List[HtmlUpdate] = {
    println(s"getting corpusEntry '${corpusEntryId}'")
    (for {
       entry <- corpus.entry(corpusEntryId).toList
     } yield {
       val initHtml = new html.SvgOverviewView().initPageImages(entry)
       HtmlReplaceInner("#main", initHtml.toString())
     })
  }

  def jsiRectangleToLTBounds(r: Rectangle): LTBounds = {
    val x = r.minX
    val y = r.minY
    val w = r.maxX - r.minX
    val h = r.maxY - r.minY
    LTBounds(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
  }

  // def pointInside(x: Double, y: Double, b: LTBounds): Boolean = {
  //   (b.x <= x && x <= b.x+b.width) && (b.y <= y && y <= b.y+b.height)
  // }
  // def overlaps(b1: LTBounds, b2: LTBounds): Boolean = {
  //   (pointInside(b1.x, b1.y, b2)
  //     || pointInside(b1.x+b1.width, b1.y, b2)
  //     || pointInside(b1.x, b1.y+b1.height, b2)
  //     || pointInside(b1.x+b1.width, b1.y+b1.height, b2))
  // }

  def onDrawPath(artifactId: String, path: Seq[Point]): List[HtmlUpdate] = {
    println(s"""draw-path:(${artifactId}, ${path.mkString(", ")} """)
    List()
  }



  def onSelectLTBounds(artifactId: String, bbox: LTBounds): List[HtmlUpdate] = {
    println(s"draw-bbox(${artifactId}, bbox=${bbox})")
    List()
  }

  // def getDocumentOverlay(corpusEntryId: String): List[LTBounds] = {
  //   println(s"getDocumentOverlay:begin(${corpusEntryId})")

  //   (for {
  //     entry <- corpus.entry(corpusEntryId).toList
  //     artifact <- entry.getArtifact("bbox.svg")
  //     f <- artifact.asPath.toOption
  //   } yield {
  //     val corpusPath = f.relativeTo(corpus.corpusRoot)
  //     println(s"SvgOverviewServer: createView(${corpusEntryId}) path=(${corpusPath})")
  //     HtmlReplaceInner("#main", new html.SvgOverviewView().init(corpusPath.toString).toString)
  //   })


  //   val maybeOverlays = corpus
  //     .entry(corpusEntryId)
  //     .getArtifact("cermine-zones.json")
  //     .asJson
  //     .map({ jsvalue =>
  //       DocumentExtractor.loadSpatialIndices(jsvalue)
  //     })

  //   val overlays = maybeOverlays.recover({ case err =>
  //     sys.error(s"getDocumentOverlay: error loading spatialindex ${err}")
  //   }).get

  //   // TODO take out hardcoded kludge
  //   // val maxLTBounds = LTBounds(
  //   val maxBBox = LTBounds(
  //     0, 0, 2000, 2000
  //   )
  //   // val combinedOverlay = concatVertical(overlays)
  //   val zones = overlays.query(PageID(0), maxBBox)

  //   val bboxes = for {
  //     zone <- zones
  //     region <- zone.regions
  //   } yield {
  //     // region.bbox
  //     val zlabels = overlays.getZoneLabels(zone.id)
  //     val lstr = zlabels.mkString("[", "; ", "]")
  //     LTBounds(
  //       x = region.bbox.left,
  //       y =  region.bbox.top,
  //       width = region.bbox.width,
  //       height = region.bbox.height,
  //       lstr
  //     )
  //   }


  //   bboxes.toList

  // }


  // def concatVertical(pages: Seq[ZoneIndexer]): ZoneIndexer = {
  //   zoneIndexer.headOption.getOrElse(sys.error("concat vertical"))
  // }
}


// (function() {
//   var canvas = this.__canvas = new fabric.Canvas('c');
//   fabric.Object.prototype.originX = fabric.Object.prototype.originY = 'center';
//   fabric.Object.prototype.transparentCorners = false;

//   fabric.loadSVGFromURL('../lib/tiger2.svg', function(objects, options) {
//     var obj = fabric.util.groupSVGElements(objects, options);
//     obj.scale(0.5);

//     // load shapes
//     for (var i = 1; i < 4; i++) {
//       for (var j = 1; j < 4; j++) {
//         obj.clone(function(i, j) {
//           return function(clone) {
//             clone.set({
//               left: i * 200 - 100,
//               top: j * 200 - 100
//             });
//             canvas.add(clone);
//             animate(clone);
//           };
//         }(i, j));
//       }
//     }
//   });

//   function animate(obj) {
//     obj.setAngle(0).animate({ angle: 360 }, {
//       duration: 3000,
//       onComplete: function(){ animate(obj) },
//       easing: function(t, b, c, d) { return c*t/d + b }
//     });
//   }

//   function cache() {
//     canvas.forEachObject(function(obj, i) {
//       if (obj.type === 'image') return;

//       var scaleX = obj.scaleX;
//       var scaleY = obj.scaleY;

//       canvas.remove(obj);
//       obj.scale(1).cloneAsImage(function(clone) {
//         clone.set({
//           left: obj.left,
//           top: obj.top,
//           scaleX: scaleX,
//           scaleY: scaleY
//         });
//         canvas.insertAt(clone, i);
//         animate(clone);
//       });
//     });
//   }

//   (function render(){
//     canvas.renderAll();
//     fabric.util.requestAnimFrame(render);
//   })();

//   document.getElementById('cache').onclick = cache;
// })();
