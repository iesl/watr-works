package edu.umass.cs.iesl.watr
package docseg

// import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import watrmarks._

import scalaz.@@
// import com.itextpdf.text.Rectangle
// import com.itextpdf.text.exceptions.InvalidPdfException
// import com.itextpdf.text.pdf._
// import com.itextpdf.text.pdf.parser.{Vector => PVector, RenderListener, _}
// import pl.edu.icm.cermine.exception.AnalysisException
// import pl.edu.icm.cermine.structure.CharacterExtractor
// import _root_.pl.edu.icm.cermine.structure.model._
// import pl.edu.icm.cermine.structure.model.BxPage
// import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import pl.edu.icm.cermine.tools.Histogram
import pl.edu.icm.cermine.tools.DisjointSets
import Bounds._
import scala.collection.JavaConversions._

class DocstrumSegmenter(
  pages: ZoneIndexer
) {

  val LB = StandardLabels

  val MAX_ZONES_PER_PAGE = 300;
  val PAGE_MARGIN = 2;
  val ORIENTATION_MARGIN = 0.2d;
  val LINES_PER_PAGE_MARGIN = 100;

  var docOrientation: Double = Double.NaN

  // import scala.collection.mutable

  // val componentMap = mutable.HashMap[BxPage, List[Component]]();
  // val componentMap = mutable.HashMap[Int@@PageID, List[Component]]();

  // input pages should include all chars
  def segmentDocument(): Unit = {
    // computeDocumentOrientation();
    // BxDocument output = new BxDocument();
    docOrientation = computeInitialOrientation()

    for (pageId <- pages.getPages) {
      val segmentedPage = segmentPage(pageId);
      segmentedPage.foreach { zonecc =>
        println(s"zone: ${zonecc.toText}")
      }
      // if (segmentedPage.getBounds() != null) {
      //   output.addPage(segmentedPage);
      // }
    }
    // output;
  }


  def segmentPage(pageId: Int@@PageID): Seq[ConnectedComponents] = {
    println(s"segmenting page ${pageId}")
    // val components = componentMap(pageId);
    if (docOrientation.isNaN()) {
      docOrientation = computeInitialOrientation();
    }

    val characterSpacing = computeCharacterSpacing(docOrientation);
    val lineSpacing = computeLineSpacing(docOrientation);

    val lines = determineLines(
      pageId,
      characterSpacing * componentDistanceCharacterMultiplier,
      lineSpacing * maxVerticalComponentDistanceMultiplier
    );

    // TODO this tries to recompute lines in the face of wonky non-zero orientation
    // if (Math.abs(orientation) > ORIENTATION_MARGIN) {
    //     List[ConnectedComponents] linesZero = determineLines(components, 0,
    //         characterSpacing * componentDistanceCharacterMultiplier,
    //         lineSpacing * maxVerticalComponentDistanceMultiplier);

    //     if (Math.abs(lines.size() - LINES_PER_PAGE_MARGIN) > Math.abs(linesZero.size() - LINES_PER_PAGE_MARGIN)) {
    //         orientation = 0;
    //         lines = linesZero;
    //     }
    // }

    val lineOrientation = computeOrientation(lines);
    println(s"lineOrientation= ${lineOrientation}")
    // if (!Double.isNaN(lineOrientation)) {
    //     orientation = lineOrientation;
    // }

    var zones = determineZones(lines, docOrientation,
      characterSpacing * minHorizontalDistanceMultiplier, Double.PositiveInfinity,
      lineSpacing * minVerticalDistanceMultiplier, lineSpacing * maxVerticalDistanceMultiplier,
      characterSpacing * minHorizontalMergeDistanceMultiplier, 0.0,
      0.0, lineSpacing * maxVerticalMergeDistanceMultiplier
    );

    // println(s"Zones")
    // zones.foreach{ zone =>
    //   println(s"  -------zone")
    //   zone.foreach { line =>
    //     val str = line.components.map(_.toText).mkString
    //     println(s"     ${str}")
    //   }
    // }

    // zones = mergeZones(zones, characterSpacing * 0.5);
    // zones = mergeLines(
    //   zones, orientation,
    //   Double.NEGATIVE_INFINITY, 0.0,
    //   0.0, lineSpacing * maxVerticalMergeDistanceMultiplier
    // )

    pageZonesToSortedZones(zones)
  }

  /**
    * Constructs sorted by x coordinate array of components from page's chunks.
    *
    * @param page page containing chunks
    * @return array of components
    */
  // protected List[Component] createComponents(BxPage page) throws AnalysisException {
  def createComponents(pageId: Int@@PageID): Unit = {
    // val chunks: List[Chunk] = List(page.getChunks())
    // val components = Array[Component](chunks.size())
    // for (int i = 0; i < components.length; i++) {
    //   try {
    //     components[i] = new Component(chunks.get(i));
    //   } catch(IllegalArgumentException ex) {
    //     throw new AnalysisException(ex);
    //   }
    // }

    //   Arrays.sort(components, ComponentXComparator.getInstance());



    // findNeighbors(pageId)
  }

  // private def findNeighbors(pageId: Int@@PageID, qbox: LTBounds): Seq[Int@@CharID] = {
  private def findNeighbors(pageId: Int@@PageID, qbox: CharBox): Seq[Int@@CharID] = {
    val hits = pages.nearestNChars(pageId, qbox.bbox, 5, 16.0f)
    hits.filter(_ != qbox.id)
  }

  /**
    * Performs for each component search for nearest-neighbors and stores the
    * result in component's neighbors attribute.
    */
  // private void findNeighbors(Component[] components) throws AnalysisException {
  def findNeighbors(pageId: Int@@PageID): Unit =  {
    //         if (components.length == 0) {
    //             return;
    //         }
    //         if (components.length == 1) {
    //             components[0].setNeighbors(new ArrayList[Neighbor]());
    //             return;
    //         }
    //         int pageNeighborCount = neighborCount; // == DEFAULT_NEIGHBOR_COUNT = 5
    //         if (components.length <= neighborCount) {
    //             pageNeighborCount = components.length - 1;
    //         }
    //
    //         List[Neighbor] candidates = new ArrayList[Neighbor]();
    //         for (int i = 0; i < components.length; i++) {
    //             int start = i, end = i + 1;
    //             // Contains components from components array
    //             // from ranges [start, i) and [i+1, end)
    //             double dist = Double.POSITIVE_INFINITY;
    //             for (double searchDist = 0; searchDist < dist; ) {
    //                 searchDist += DISTANCE_STEP; ///  = 16.0;
    //                 boolean newCandidatesFound = false;
    //
    //                 while (start > 0 && components[i].getX() - components[start - 1].getX() < searchDist) {
    //                     start--;
    //                     candidates.add(new Neighbor(components[start], components[i]));
    //                     if (candidates.size() > pageNeighborCount) {
    //                         Collections.sort(candidates, NeighborDistanceComparator.getInstance());
    //                         candidates.subList(pageNeighborCount, candidates.size()).clear();
    //                     }
    //                     newCandidatesFound = true;
    //                 }
    //                 while (end < components.length && components[end].getX() - components[i].getX() < searchDist) {
    //                     candidates.add(new Neighbor(components[end], components[i]));
    //                     if (candidates.size() > pageNeighborCount) {
    //                         Collections.sort(candidates, NeighborDistanceComparator.getInstance());
    //                         candidates.subList(pageNeighborCount, candidates.size()).clear();
    //                     }
    //                     end++;
    //                     newCandidatesFound = true;
    //                 }

    //                 if (newCandidatesFound && candidates.size() >= pageNeighborCount) {
    //                     Collections.sort(candidates, NeighborDistanceComparator.getInstance());
    //                     dist = candidates.get(pageNeighborCount - 1).getDistance();
    //                 }
    //             }
    //             candidates.subList(pageNeighborCount, candidates.size()).clear();
    //             components[i].setNeighbors(new ArrayList[Neighbor>(candidates));
    //             candidates.clear();
    //         }
  }



    //     /**
    //      * Computes initial orientation estimation based on nearest-neighbors' angles.
    //      *
    //      * @param components
    //      * @return initial orientation estimation
    //      */
    // private double computeInitialOrientation(List[Component] components) {
  def computeInitialOrientation(): Double = {
    println(s"computeInitialOrientation")
    val histPeaks = for {
      pageId <- pages.getPages
    } yield {
      val histogram = new Histogram(-Math.PI/2, Math.PI/2, angleHistogramResolution);
      println(s"hist range = ${-Math.PI/2} - ${Math.PI/2}")
      for {
        component <- pages.getComponents(pageId)
      } yield {

        // println(s"neighbors of ${component.char}")
        val neighbors = findNeighbors(pageId, component)
        neighbors
          .map(pages.getComponent(pageId, _))
          .foreach({c =>
            val angle = c.bbox.toCenterPoint.angleTo(component.bbox.toCenterPoint)
            val dist = c.bbox.centerDistanceTo(component.bbox)
            // println(s"    ${c.char}: ${c.bbox.prettyPrint} -> ${component.bbox.prettyPrint}")
            // println(s"        angle=${angle} dist=${dist}")
            histogram.add(angle)
          })
      }
      // Rectangular smoothing window has been replaced with gaussian smoothing window
      histogram.circularGaussianSmooth(angleHistogramSmoothingWindowLength, angleHistogramSmoothingWindowStdDeviation)

      histogram.getPeakValue()
    }

    println(s"""computeInitialOrientation: ${histPeaks.mkString(", ")}""")

    histPeaks.headOption.getOrElse(0.0)
  }


  /**
    * Computes within-line spacing based on nearest-neighbors distances.
    */
  // private double computeCharacterSpacing(List[Component] components, double orientation) {
  def computeCharacterSpacing(orientation: Double): Double = {
    computeSpacing(orientation)
  }

    /**
     * Computes between-line spacing based on nearest-neighbors distances.
     *
      * @param components
      * @param orientation estimated text orientation
      * @return estimated between-line spacing
      */
    // private double computeLineSpacing(List[Component] components, double orientation) {
  def computeLineSpacing(orientation: Double): Double = {
    if (orientation >= 0) {
      computeSpacing(orientation - Math.PI / 2);
    } else {
      computeSpacing(orientation + Math.PI / 2);
    }
  }





  //     private double computeSpacing(List[Component] components, double angle) {
  def computeSpacing(angle: Double): Double = {
    // TODO: should filter based on angle when computing max distance
    var maxDistance = Double.NegativeInfinity
    for {
      pageId <- pages.getPages
      component <- pages.getComponents(pageId)
    } yield {
      // println(s"computing spacing page ${pageId}, ${component.char}: ${component.bbox.prettyPrint}")
      val neighbors = findNeighbors(pageId, component)
      neighbors.map(pages.getComponent(pageId, _)).foreach {n =>
        val dist = n.bbox.centerDistanceTo(component.bbox)
        maxDistance = math.max(maxDistance, dist);
      }

      // val pp = neighbors.map(pages.getComponent(pageId, _)).map{c =>
      //   val dist = c.bbox.centerDistanceTo(component.bbox)
      //   s"${c.char}: ${c.bbox.prettyPrint} dist=${dist}"
      // }.mkString("\n  ", "\n  ", "\n")

      // println(s"neighbors of ${component.char}${pp}")
    }



    val histPeaks = for {
      pageId <- pages.getPages
    } yield {
      val histogram = new Histogram(0, maxDistance, spacingHistogramResolution);
      val angleFilter = AngleFilter(angle - angleTolerance, angle + angleTolerance);

      for {
        component <- pages.getComponents(pageId)
      } yield {

        // println(s"neighbors of ${component.char}")
        val neighbors = findNeighbors(pageId, component)

        neighbors
          .map(pages.getComponent(pageId, _))
          .filter({c =>
            val dist = c.bbox.centerDistanceTo(component.bbox)
            val angle = component.bbox.toCenterPoint.angleTo(c.bbox.toCenterPoint)
            val ok = angleFilter.matches(angle)

            // println(s"    ${c.char}: ${c.bbox.prettyPrint} <-> ${component.bbox.prettyPrint}")
            // println(s"        allow($ok): angle=${angle}, dist=${dist}")
            ok
          })
          .foreach({c =>
            val dist = component.bbox.centerDistanceTo(c.bbox)
            histogram.add(dist)
          })
      }

      // Rectangular smoothing window has been replaced with gaussian smoothing window
      histogram.gaussianSmooth(
        spacingHistogramSmoothingWindowLength,
        spacingHistogramSmoothingWindowStdDeviation);

      histogram.getPeakValue()
    }

    println(s"computing spacing w/angle = ${angle}, max distance = ${maxDistance}")
    println(s"     hist peaks = ${histPeaks.mkString(", ")}")

    histPeaks.headOption.getOrElse(0.0d)

  }

// for (Component component : components) {
//     for (Neighbor neighbor : component.getNeighbors()) {
//         if (filter.matches(neighbor)) {
//             histogram.add(neighbor.getDistance());
//         }
//     }
// }
// // Rectangular smoothing window has been replaced with gaussian smoothing window
// histogram.gaussianSmooth(spacingHistogramSmoothingWindowLength,
//         spacingHistogramSmoothingWindowStdDeviation);
// return histogram.getPeakValue();


    // double maxDistance = Double.NEGATIVE_INFINITY;
    // for (Component component : components) {
    //     for (Neighbor neighbor : component.getNeighbors()) {
    //         maxDistance = Math.max(maxDistance, neighbor.getDistance());
    //     }
    // }
    // Histogram histogram = new Histogram(0, maxDistance, spacingHistogramResolution);
    // AngleFilter filter = AngleFilter.newInstance(angle - angleTolerance, angle + angleTolerance);
    // for (Component component : components) {
    //     for (Neighbor neighbor : component.getNeighbors()) {
    //         if (filter.matches(neighbor)) {
    //             histogram.add(neighbor.getDistance());
    //         }
    //     }
    // }
    // // Rectangular smoothing window has been replaced with gaussian smoothing window
    // histogram.gaussianSmooth(spacingHistogramSmoothingWindowLength,
    //         spacingHistogramSmoothingWindowStdDeviation);
    // return histogram.getPeakValue();
    //   0d
    // }


    /**
     * Groups components into text lines.
     *
     * @param components
     * @param orientation - estimated text orientation
     * @param maxHorizontalDistance - maximum horizontal distance between components
     * @param maxVerticalDistance - maximum vertical distance between components
     * @return lines of components
     */
    // private List[ConnectedComponents] determineLines(List[Component] components, double orientation, double maxHorizontalDistance, double maxVerticalDistance) {
  def determineLines(
    pageId: Int@@PageID,
    maxHorizontalDistance: Double,
    maxVerticalDistance: Double
  ): Seq[ConnectedComponents] = {
    println(s"""determineLines(maxHorizontalDistance:${maxHorizontalDistance}, maxVerticalDistance: ${maxVerticalDistance})""")
    // val sets = new DisjointSets[CharBox](components);
    val angleFilter = AngleFilter(docOrientation - angleTolerance, docOrientation + angleTolerance);

    val sets = new DisjointSets[CharBox](pages.getComponents(pageId));
    println(s"""    determineLines(page ${pageId})""")

    for {
      component <- pages.getComponents(pageId)
    } {
      findNeighbors(pageId, component)
        .map(pages.getComponent(pageId, _))
        .foreach({neigh =>
          val angle = component.bbox.toCenterPoint.angleTo(neigh.bbox.toCenterPoint)

          val dx = neigh.bbox.toCenterPoint.hdist(component.bbox.toCenterPoint) / maxHorizontalDistance
          val dy = neigh.bbox.toCenterPoint.vdist(component.bbox.toCenterPoint) / maxVerticalDistance
          val matches = angleFilter.matches(angle)

          // println(s"""    angle:${angle} dx:$dx dy:$dy  match:$matches""")

          if (angleFilter.matches(angle) && dx*dx + dy*dy <= 1) {
            sets.union(component, neigh);
          }
        })
    }

    sets.iterator().toSeq.map{ group =>
      val gset = group.toSeq.sortBy(_.bbox.left).map(Component(_))
      Component(gset, docOrientation)
    }
  }

  // private double computeOrientation(List[ConnectedComponents] lines) {
  def computeOrientation(lines: Seq[ConnectedComponents]): Double = {
    // Compute weighted mean of line angles
    var valueSum = 0.0;
    var weightSum = 0.0;
    for (line <- lines) {
      valueSum += line.getAngle() * line.getLength();
      weightSum += line.getLength();
    }
    return valueSum / weightSum;
  }

      /**
       * Groups text lines into zones.
       *
       * @param lines
       * @param orientation
       * @param minHorizontalDistance
       * @param maxHorizontalDistance
       * @param minVerticalDistance
       * @param maxVerticalDistance
       * @param minHorizontalMergeDistance
       * @param maxHorizontalMergeDistance
       * @param minVerticalMergeDistance
       * @param maxVerticalMergeDistance
       * @return
       */
  def determineZones(
    lines: Seq[ConnectedComponents], orientation: Double,
    minHorizontalDistance: Double, maxHorizontalDistance: Double,
    minVerticalDistance: Double, maxVerticalDistance: Double,
    minHorizontalMergeDistance: Double, maxHorizontalMergeDistance: Double,
    minVerticalMergeDistance: Double, maxVerticalMergeDistance: Double
  ): Seq[Seq[ConnectedComponents]] = {
    val sets = new DisjointSets[ConnectedComponents](lines);
    // Mean height is computed so that all distances can be scaled
    // relative to the line height
    var meanHeight = 0.0
    var weights = 0.0;


    for (line <- lines) {
      var weight = line.getLength();
      meanHeight += line.height * weight;
      weights += weight;
    }
    meanHeight /= weights;

    for {
      tail <- lines.tails
      li <- tail.headOption.toSeq
      lj <- tail.drop(1)
    } {
      var scale = Math.min(li.height, lj.height) / meanHeight;
      scale = Math.max(minLineSizeScale, Math.min(scale, maxLineSizeScale));
      // "<=" is used instead of "<" for consistency and to allow setting minVertical(Merge)Distance
      // to 0.0 with meaning "no minimal distance required"
      if (!sets.areTogether(li, lj) && li.angularDifference(lj) <= angleTolerance) {
        val hDist = li.horizontalDistance(lj, orientation) / scale;
        val vDist = li.verticalDistance(lj, orientation) / scale;
        // Line over or above
        if (minHorizontalDistance <= hDist && hDist <= maxHorizontalDistance
          && minVerticalDistance <= vDist && vDist <= maxVerticalDistance) {
          sets.union(li, lj);
        }
        // Split line that needs later merging
        else if (minHorizontalMergeDistance <= hDist && hDist <= maxHorizontalMergeDistance
          && minVerticalMergeDistance <= vDist && vDist <= maxVerticalMergeDistance) {
          sets.union(li, lj);
        }
      }

    }

    val zones = sets.iterator().toSeq.map{ group =>
      group.toSeq.sortBy(_.y0)
    }
    zones
  }

  import scala.collection.mutable

  // def mergeZones(zones: List[List[ConnectedComponents]], tolerance: Double): List[List[ConnectedComponents]] = {
  //   // val bounds = mutable.ArrayBuffer[BxBounds](zones.size());
  //   for (zone <- zones) {
  //     val builder = new BxBoundsBuilder();
  //     for (line <- zone) {
  //       for (component <- line.components) {
  //         // builder.expand(component.getChunk().getBounds());
  //         builder.expand(component.bbox);
  //       }
  //     }
  //     bounds.add(builder.getBounds());
  //   }

  //   // List[List[ConnectedComponents]] outputZones = new ArrayList[List[ConnectedComponents]]();
  //   val outputZones = mutable.ArrayBuffer[Seq[ConnectedComponents]]()

  //   // mainFor:
  //   for (int i = 0; i < zones.size(); i++) {
  //     for (int j = 0; j < zones.size(); j++) {
  //       if (i == j || bounds.get(j) == null || bounds.get(i) == null) {
  //         continue;
  //       }
  //       if (BxModelUtils.contains(bounds.get(j), bounds.get(i), tolerance)) {
  //         zones.get(j).addAll(zones.get(i));
  //         bounds.set(i, null);
  //         continue mainFor;
  //       }
  //     }
  //     outputZones.add(zones.get(i));
  //   }
  //   return outputZones;
  // }

  //     private List[List[ConnectedComponents]] mergeLines(List[List[ConnectedComponents]] zones, double orientation,
  //             double minHorizontalDistance, double maxHorizontalDistance,
  //             double minVerticalDistance, double maxVerticalDistance) {
  //         List[List[ConnectedComponents]] outputZones = new ArrayList[List[ConnectedComponents]](zones.size());
  //         for (List[ConnectedComponents] zone : zones) {
  //             outputZones.add(mergeLinesInZone(zone, orientation,
  //                 minHorizontalDistance, maxHorizontalDistance,
  //                 minVerticalDistance, maxVerticalDistance));
  //         }
  //         return outputZones;
  //     }

  //     private List[ConnectedComponents] mergeLinesInZone(List[ConnectedComponents] lines, double orientation,
  //             double minHorizontalDistance, double maxHorizontalDistance,
  //             double minVerticalDistance, double maxVerticalDistance) {
  //         DisjointSets[ConnectedComponents] sets = new DisjointSets[ConnectedComponents](lines);
  //         for (int i = 0; i < lines.size(); i++) {
  //             ConnectedComponents li = lines.get(i);
  //             for (int j = i + 1; j < lines.size(); j++) {
  //                 ConnectedComponents lj = lines.get(j);
  //                 double hDist = li.horizontalDistance(lj, orientation);
  //                 double vDist = li.verticalDistance(lj, orientation);
  //                 if (minHorizontalDistance <= hDist && hDist <= maxHorizontalDistance
  //                         && minVerticalDistance <= vDist && vDist <= maxVerticalDistance) {
  //                     sets.union(li, lj);
  //                 } else if (minVerticalDistance <= vDist && vDist <= maxVerticalDistance
  //                         && Math.abs(hDist-Math.min(li.getLength(), lj.getLength())) < 0.1) {
  //                     boolean componentOverlap = false;
  //                     int overlappingCount = 0;
  //                     for (Component ci : li.getComponents()) {
  //                         for (Component cj : lj.getComponents()) {
  //                             double dist = ci.overlappingDistance(cj, orientation);
  //                             if (dist > 2) {
  //                                 componentOverlap = true;
  //                             }
  //                             if (dist > 0) {
  //                                 overlappingCount++;
  //                             }
  //                         }
  //                     }
  //                     if (!componentOverlap && overlappingCount <= 2) {
  //                         sets.union(li, lj);
  //                     }
  //                 }
  //             }
  //         }
  //         List[ConnectedComponents] outputZone = new ArrayList[ConnectedComponents]();
  //         for (Set[ConnectedComponents] group : sets) {
  //             List[Component] components = new ArrayList[Component]();
  //             for (ConnectedComponents line : group) {
  //                 components.addAll(line.getComponents());
  //             }
  //             Collections.sort(components, ComponentXComparator.getInstance());
  //             outputZone.add(new ConnectedComponents(components, orientation));
  //         }
  //         return outputZone;
  //     }

  /**
    * Converts list of zones from internal format (using components and
    * component lines) to BxModel.
    *
    * @param zones zones in internal format
    * @param wordSpacing - maximum allowed distance between components that
    * belong to one word
    * @return BxModel page
    */
  def pageZonesToSortedZones(zones: Seq[Seq[ConnectedComponents]]): Seq[ConnectedComponents] = {
    // BxPage page = new BxPage();
    // List[BxPage] pages = Lists.newArrayList(origPage.getParent());
    // int pageIndex = pages.indexOf(origPage);
    // boolean groupped = false;
    // if (zones.size() > MAX_ZONES_PER_PAGE && pageIndex >= PAGE_MARGIN
    //         && pageIndex < pages.size() - PAGE_MARGIN) {
    //     val oneZone:List[ConnectedComponents]  = List[ConnectedComponents]();
    //     for (List[ConnectedComponents] zone : zones) {
    //         oneZone.addAll(zone);
    //     }
    //     zones = new ArrayList[List[ConnectedComponents]]();
    //     zones.add(oneZone);
    //     groupped = true;
    // }
    val page = for (lines <- zones) yield {
      val zLines = mutable.ArrayBuffer[ConnectedComponents]()
      // if (groupped) {
      //     zone.setLabel(BxZoneLabel.GEN_OTHER);
      // }
      for (line <- lines) {
        zLines.append(line.convertToBxLine());
      }

      val zSorted = zLines.sortWith({ case (cc1, cc2) =>
        cc1.bounds.top < cc2.bounds.top
      })


      // zone.setLines(zLines);
      // BxBoundsBuilder.setBounds(zone);
      // page.addZone(zone);
      zSorted.toSeq
    }

    val pccs = page.map({case ccs => new ConnectedComponents(ccs, 0d, Some(LB.Zone)) })

    val sortedZones = sortZonesYX(pccs)
    
    sortedZones.foreach { cc => println(s"zone ${cc.toText}") }
    sortedZones
    // BxBoundsBuilder.setBounds(page);
    // return page;
  }

  def compareDouble(d1: Double, d2: Double, precision: Double): Int = {
    if (d1.isNaN() || d2.isNaN()) {
      d1.compareTo(d2)
    } else {
      val p:Double = if (precision == 0) 0 else 1

      val i1: Long = math.round(d1 / p);
      val i2: Long = math.round(d2 / p);
      i1.compareTo(i2)
    }
  }

  def sortZonesYX(zones: Seq[ConnectedComponents]): Seq[ConnectedComponents]= {

    zones.sortWith({case (cc1, cc2) =>
      val ycmp = compareDouble(cc1.bounds.top, cc2.bounds.top, 0.01)

      val cmp = if (ycmp == 0) {
        compareDouble(cc1.bounds.left, cc2.bounds.left, 0.01)
      } else {
        ycmp
      }

      cmp < 0
    })
  }

  val DEFAULT_SPACE_HIST_RES: Double = 0.5
  val DISTANCE_STEP: Double = 16.0;

  val DEFAULT_ANGLE_HIST_RES : Double = Math.toRadians(0.5);
  val DEFAULT_ANGLE_HIST_SMOOTH_LEN : Double = 0.25 * Math.PI;
  val DEFAULT_ANGLE_HIST_SMOOTH_STDDEV : Double = 0.0625 * Math.PI;
  val DEFAULT_SPACE_HIST_SMOOTH_LEN : Double = 2.5;
  val DEFAULT_SPACE_HIST_SMOOTH_STDDEV : Double = 0.5;
  val DEFAULT_MAX_VERT_COMP_DIST : Double = 0.67;
  val DEFAULT_MIN_LINE_SIZE_SCALE : Double = 0.9;
  val DEFAULT_MAX_LINE_SIZE_SCALE : Double = 2.5;
  val DEFAULT_MIN_HORIZONTAL_DIST : Double = -0.5;
  val DEFAULT_MIN_VERTICAL_DIST : Double = 0.0;
  val DEFAULT_MAX_VERTICAL_DIST : Double = 1.2;
  val DEFAULT_COMP_DIST_CHAR : Double = 3.5;
  val DEFAULT_WORD_DIST : Double = 0.2;
  val DEFAULT_MIN_HORIZONTAL_MERGE_DIST : Double = -3.0;
  val DEFAULT_MAX_VERTICAL_MERGE_DIST : Double = 0.5;
  val DEFAULT_NEIGHBOR_COUNT : Double = 5;

  val DEFAULT_ANGLE_TOLERANCE: Double = Math.PI / 6;



  //     /**
  //      * Angle histogram resolution in radians per bin.
  //      */
  val angleHistogramResolution = DEFAULT_ANGLE_HIST_RES

  //     /**
  //      * Angle histogram smoothing window length in radians.
  //      * Length of angle histogram is equal to pi.
  //      */
      val angleHistogramSmoothingWindowLength: Double = DEFAULT_ANGLE_HIST_SMOOTH_LEN;

  //     /**
  //      * Angle histogram gaussian smoothing window standard deviation in radians.
  //      */
      val angleHistogramSmoothingWindowStdDeviation:Double = DEFAULT_ANGLE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Spacing histogram resolution per bin.
  //      */
      val spacingHistogramResolution: Double = DEFAULT_SPACE_HIST_RES;

  //     /**
  //      * Spacing histogram smoothing window length.
  //      */
      val spacingHistogramSmoothingWindowLength:Double = DEFAULT_SPACE_HIST_SMOOTH_LEN;

  //     /**
  //      * Spacing histogram gaussian smoothing window standard deviation.
  //      */
      val spacingHistogramSmoothingWindowStdDeviation:Double = DEFAULT_SPACE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Maximum vertical component distance multiplier used during line
  //      * determination.
  //      *
  //      * Maximum vertical distance between components (characters) that belong
  //      * to the same line is equal to the product of this value and estimated
  //      * between-line spacing.
  //      */
      val maxVerticalComponentDistanceMultiplier:Double = DEFAULT_MAX_VERT_COMP_DIST;

  //     /**
  //      * Minimum line size scale value.
  //      *
  //      * During zone determination (merging lines into zones) line height is
  //      * taken into account. To achieve this, line size scale is estimated and
  //      * limited to range [minLineSizeScale, maxLineSizeScale].
  //      */
      val minLineSizeScale:Double = DEFAULT_MIN_LINE_SIZE_SCALE;

  //     /**
  //      * Maximum line size scale value.
  //      *
  //      * See minLineSizeScale for more information.
  //      */
      val maxLineSizeScale:Double = DEFAULT_MAX_LINE_SIZE_SCALE;

  //     /**
  //      * Minimum horizontal line distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated within-line spacing.
  //      */
      val minHorizontalDistanceMultiplier:Double = DEFAULT_MIN_HORIZONTAL_DIST;

  //     /**
  //      * Minimum vertical line distance multiplier.
  //      *
  //      * Minimum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
      val minVerticalDistanceMultiplier:Double = DEFAULT_MIN_VERTICAL_DIST;

  //     /**
  //      * Maximum vertical line distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
      val maxVerticalDistanceMultiplier:Double = DEFAULT_MAX_VERTICAL_DIST;

  //     /**
  //      * Component distance character spacing multiplier.
  //      *
  //      * Maximum distance between components that belong to the same line is
  //      * equal to (lineSpacing * componentDistanceLineMultiplier +
  //      * characterSpacing * componentDistanceCharacterMultiplier), where
  //      * lineSpacing and characterSpacing are estimated between-line and
  //      * within-line spacing, respectively.
  //      */
      val componentDistanceCharacterMultiplier:Double = DEFAULT_COMP_DIST_CHAR;

  //     /**
  //      * Word distance multiplier.
  //      *
  //      * Maximum distance between components that belong to the same word is
  //      * equal to the product of this value and estimated within-line spacing.
  //      */
      val wordDistanceMultiplier:Double = DEFAULT_WORD_DIST;

  //     /**
  //      * Minimum horizontal line merge distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that should be merged is equal
  //      * to the product of this value and estimated within-line spacing.
  //      *
  //      * Because split lines do not overlap this value should be negative.
  //      */

      val minHorizontalMergeDistanceMultiplier:Double = DEFAULT_MIN_HORIZONTAL_MERGE_DIST;

  //     /**
  //      * Maximum vertical line merge distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that should be merged is equal
  //      * to the product of this value and estimated between-line spacing.
  //      */

      val maxVerticalMergeDistanceMultiplier:Double = DEFAULT_MAX_VERTICAL_MERGE_DIST;

  //     /**
  //      * Angle tolerance for comparisons of angles between components and angles
  //      * between lines.
  //      */
      val angleTolerance: Double = DEFAULT_ANGLE_TOLERANCE;

  //     /**
  //      * Number of nearest-neighbors found per component.
  //      */
  //     private int neighborCount = DEFAULT_NEIGHBOR_COUNT;


  //     public void setSpacingHistogramResolution(double value) {
  //         spacingHistogramResolution = value;
  //     }

  //     public void setSpacingHistogramSmoothingWindowLength(double value) {
  //         spacingHistogramSmoothingWindowLength = value;
  //     }

  //     public void setSpacingHistogramSmoothingWindowStdDeviation(double value) {
  //         spacingHistogramSmoothingWindowStdDeviation = value;
  //     }

  //     public void setMaxLineSizeScale(double value) {
  //         maxLineSizeScale = value;
  //     }

  //     public void setMaxVerticalDistanceMultiplier(double value) {
  //         maxVerticalDistanceMultiplier = value;
  //     }

  //     public void setMinHorizontalDistanceMultiplier(double value) {
  //         minHorizontalDistanceMultiplier = value;
  //     }

  //     public void setComponentDistanceCharacterMultiplier(double value) {
  //         componentDistanceCharacterMultiplier = value;
  //     }

  //     public void setWordDistanceMultiplier(double value) {
  //         wordDistanceMultiplier = value;
  //     }

  //     public void setMaxVerticalMergeDistanceMultiplier(double value) {
  //         maxVerticalMergeDistanceMultiplier = value;
  //     }

  //     public void setAngleTolerance(double value) {
  //         angleTolerance = value;
  //     }

  // }

}
