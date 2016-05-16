package edu.umass.cs.iesl.watr
package ext

import watrmarks._

import scalaz.@@
// import com.itextpdf.text.Rectangle
// import com.itextpdf.text.exceptions.InvalidPdfException
// import com.itextpdf.text.pdf._
// import com.itextpdf.text.pdf.parser.{Vector => PVector, RenderListener, _}
// import pl.edu.icm.cermine.exception.AnalysisException
// import pl.edu.icm.cermine.structure.CharacterExtractor
import _root_.pl.edu.icm.cermine.structure.model._
// import pl.edu.icm.cermine.structure.model.BxPage
// import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder

class DocstrumSegmenter() {

//     public static final int MAX_ZONES_PER_PAGE = 300;
//     public static final int PAGE_MARGIN = 2;
//     public static final double ORIENTATION_MARGIN = 0.2;
//     public static final int LINES_PER_PAGE_MARGIN = 100;

  private var docOrientation: Double = Double.NaN

  import scala.collection.mutable

  // val componentMap = mutable.HashMap[BxPage, List[Component]]();
  val componentMap = mutable.HashMap[Int@@PageID, List[Component]]();

  // input pages should include all chars
  def segmentDocument(pages: ZoneIndexer): Unit = {
    computeDocumentOrientation(pages);
    // BxDocument output = new BxDocument();
    for (pageId <- pages.getPages) {
      val segmentedPage = segmentPage(pageId, pages);
      // if (segmentedPage.getBounds() != null) {
      //   output.addPage(segmentedPage);
      // }
    }
    // output;
  }

  // def computeDocumentOrientation(BxDocument document): Double = {
  def computeDocumentOrientation(pages: ZoneIndexer): Double = {
    val components = List[Component]();
    for (pageId <- pages.getPages) {
      val pageComponents = createComponents(pageId, pages);
      componentMap.put(page, pageComponents);
      components.addAll(pageComponents);
    }

    computeInitialOrientation(components);
  }


  def computeDocumentOrientation(componentMap: Map[BxPage, List[Component]]): Double = {
    // this.componentMap = componentMap;
    // List[Component] components = new ArrayList[Component]();
    // for (Map.Entry[BxPage, List[Component]] entry : componentMap.entrySet()) {
    //   components.addAll(entry.getValue());
    // }
    // computeInitialOrientation(components);
    ???
  }

  // protected segmentPage(BxPage page) throws AnalysisException {
  // import util._

  def segmentPage(pageId: Int@@PageID, pages: ZoneIndexer): Unit = {
    val components = componentMap(pageId);
    var orientation = docOrientation;
    if (orientation.isNaN()) {
        orientation = computeInitialOrientation(components);
    }
    // double characterSpacing = computeCharacterSpacing(components, orientation);
    // double lineSpacing = computeLineSpacing(components, orientation);

    // List[ComponentLine] lines = determineLines(components, orientation,
    //         characterSpacing * componentDistanceCharacterMultiplier,
    //         lineSpacing * maxVerticalComponentDistanceMultiplier);

    // if (Math.abs(orientation) > ORIENTATION_MARGIN) {
    //     List[ComponentLine] linesZero = determineLines(components, 0,
    //         characterSpacing * componentDistanceCharacterMultiplier,
    //         lineSpacing * maxVerticalComponentDistanceMultiplier);

    //     if (Math.abs(lines.size() - LINES_PER_PAGE_MARGIN) > Math.abs(linesZero.size() - LINES_PER_PAGE_MARGIN)) {
    //         orientation = 0;
    //         lines = linesZero;
    //     }
    // }

    // double lineOrientation = computeOrientation(lines);
    // if (!Double.isNaN(lineOrientation)) {
    //     orientation = lineOrientation;
    // }

    // List[List[ComponentLine]] zones = determineZones(lines, orientation,
    //         characterSpacing * minHorizontalDistanceMultiplier, Double.POSITIVE_INFINITY,
    //         lineSpacing * minVerticalDistanceMultiplier, lineSpacing * maxVerticalDistanceMultiplier,
    //         characterSpacing * minHorizontalMergeDistanceMultiplier, 0.0,
    //         0.0, lineSpacing * maxVerticalMergeDistanceMultiplier);
    // zones = mergeZones(zones, characterSpacing * 0.5);
    // zones = mergeLines(zones, orientation,
    //         Double.NEGATIVE_INFINITY, 0.0,
    //         0.0, lineSpacing * maxVerticalMergeDistanceMultiplier);
    // return convertToBxModel(page, zones, wordDistanceMultiplier * characterSpacing);
  }

  /**
    * Constructs sorted by x coordinate array of components from page's chunks.
    *
    * @param page page containing chunks
    * @return array of components
    */
  // protected List[Component] createComponents(BxPage page) throws AnalysisException {
  def createComponents(pageId: Int@@PageID, pages: ZoneIndexer) : List[Component] = {
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



      findNeighbors(components);
      components;
    }

  /**
    * Performs for each component search for nearest-neighbors and stores the
    * result in component's neighbors attribute.
    */
  // private void findNeighbors(Component[] components) throws AnalysisException {
  def findNeighbors(pageId: Int@@PageID, pages: ZoneIndexer): Unit =  {
    //         if (components.length == 0) {
    //             return;
    //         }
    //         if (components.length == 1) {
    //             components[0].setNeighbors(new ArrayList[Neighbor]());
    //             return;
    //         }
    //         int pageNeighborCount = neighborCount;
    //         if (components.length <= neighborCount) {
    //             pageNeighborCount = components.length - 1;
    //         }

    //         List[Neighbor] candidates = new ArrayList[Neighbor]();
    //         for (int i = 0; i < components.length; i++) {
    //             int start = i, end = i + 1;
    //             // Contains components from components array
    //             // from ranges [start, i) and [i+1, end)
    //             double dist = Double.POSITIVE_INFINITY;
    //             for (double searchDist = 0; searchDist < dist; ) {
    //                 searchDist += DISTANCE_STEP;
    //                 boolean newCandidatesFound = false;

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
    def computeInitialOrientation(components: List[Component]): Double = {

      // Histogram histogram = new Histogram(-Math.PI/2, Math.PI/2, angleHistogramResolution);
      // for (Component component : components) {
      //     for (Neighbor neighbor : component.getNeighbors()) {
      //         histogram.add(neighbor.getAngle());
    //     }
    // }
    // // Rectangular smoothing window has been replaced with gaussian smoothing window
    // histogram.circularGaussianSmooth(angleHistogramSmoothingWindowLength,
    //         angleHistogramSmoothingWindowStdDeviation);
    // return histogram.getPeakValue();
    ???
  }


}
  //     /**
  //      * Computes within-line spacing based on nearest-neighbors distances.
  //      *
  //      * @param components
  //      * @param orientation estimated text orientation
  //      * @return estimated within-line spacing
  //      */
  //     private double computeCharacterSpacing(List[Component] components, double orientation) {
  //         return computeSpacing(components, orientation);
  //     }

  //     /**
  //      * Computes between-line spacing based on nearest-neighbors distances.
  //      *
  //      * @param components
  //      * @param orientation estimated text orientation
  //      * @return estimated between-line spacing
  //      */
  //     private double computeLineSpacing(List[Component] components, double orientation) {
  //         if (orientation >= 0) {
  //             return computeSpacing(components, orientation - Math.PI / 2);
  //         } else {
  //             return computeSpacing(components, orientation + Math.PI / 2);
  //         }
  //     }

  //     private double computeSpacing(List[Component] components, double angle) {
  //         double maxDistance = Double.NEGATIVE_INFINITY;
  //         for (Component component : components) {
  //             for (Neighbor neighbor : component.getNeighbors()) {
  //                 maxDistance = Math.max(maxDistance, neighbor.getDistance());
  //             }
  //         }
  //         Histogram histogram = new Histogram(0, maxDistance, spacingHistogramResolution);
  //         AngleFilter filter = AngleFilter.newInstance(angle - angleTolerance, angle + angleTolerance);
  //         for (Component component : components) {
  //             for (Neighbor neighbor : component.getNeighbors()) {
  //                 if (filter.matches(neighbor)) {
  //                     histogram.add(neighbor.getDistance());
  //                 }
  //             }
  //         }
  //         // Rectangular smoothing window has been replaced with gaussian smoothing window
  //         histogram.gaussianSmooth(spacingHistogramSmoothingWindowLength,
  //                 spacingHistogramSmoothingWindowStdDeviation);
  //         return histogram.getPeakValue();
  //     }

  //     /**
  //      * Groups components into text lines.
  //      *
  //      * @param components
  //      * @param orientation - estimated text orientation
  //      * @param maxHorizontalDistance - maximum horizontal distance between components
  //      * @param maxVerticalDistance - maximum vertical distance between components
  //      * @return lines of components
  //      */
  //     private List[ComponentLine] determineLines(List[Component] components, double orientation,
  //             double maxHorizontalDistance, double maxVerticalDistance) {
  //         DisjointSets[Component] sets = new DisjointSets[Component](components);
  //         AngleFilter filter = AngleFilter.newInstance(orientation - angleTolerance, orientation + angleTolerance);
  //         for (Component component : components) {
  //             for (Neighbor neighbor : component.getNeighbors()) {
  //                 double x = neighbor.getHorizontalDistance(orientation) / maxHorizontalDistance;
  //                 double y = neighbor.getVerticalDistance(orientation) / maxVerticalDistance;
  //                 if (filter.matches(neighbor) && x * x + y * y <= 1) {
  //                     sets.union(component, neighbor.getComponent());
  //                 }
  //             }
  //         }
  //         List[ComponentLine] lines = new ArrayList[ComponentLine]();
  //         for (Set[Component] group : sets) {
  //           List[Component] lineComponents = new ArrayList[Component](group);
  //           Collections.sort(lineComponents, ComponentXComparator.getInstance());
  //           lines.add(new ComponentLine(lineComponents, orientation));
  //         }
  //         return lines;
  //     }

  //     private double computeOrientation(List[ComponentLine] lines) {
  //         // Compute weighted mean of line angles
  //         double valueSum = 0.0;
  //         double weightSum = 0.0;
  //         for (ComponentLine line : lines) {
  //             valueSum += line.getAngle() * line.getLength();
  //             weightSum += line.getLength();
  //         }
  //         return valueSum / weightSum;
  //     }

  //     /**
  //      * Groups text lines into zones.
  //      *
  //      * @param lines
  //      * @param orientation
  //      * @param minHorizontalDistance
  //      * @param maxHorizontalDistance
  //      * @param minVerticalDistance
  //      * @param maxVerticalDistance
  //      * @param minHorizontalMergeDistance
  //      * @param maxHorizontalMergeDistance
  //      * @param minVerticalMergeDistance
  //      * @param maxVerticalMergeDistance
  //      * @return
  //      */
  //     private List[List[ComponentLine]] determineZones(List[ComponentLine] lines, double orientation,
  //             double minHorizontalDistance, double maxHorizontalDistance,
  //             double minVerticalDistance, double maxVerticalDistance,
  //             double minHorizontalMergeDistance, double maxHorizontalMergeDistance,
  //             double minVerticalMergeDistance, double maxVerticalMergeDistance) {
  //         DisjointSets[ComponentLine] sets = new DisjointSets[ComponentLine](lines);
  //         // Mean height is computed so that all distances can be scaled
  //         // relative to the line height
  //         double meanHeight = 0.0, weights = 0.0;
  //         for (ComponentLine line : lines) {
  //             double weight = line.getLength();
  //             meanHeight += line.getHeight() * weight;
  //             weights += weight;
  //         }
  //         meanHeight /= weights;

  //         for (int i = 0; i < lines.size(); i++) {
  //             ComponentLine li = lines.get(i);
  //             for (int j = i + 1; j < lines.size(); j++) {
  //                 ComponentLine lj = lines.get(j);
  //                 double scale = Math.min(li.getHeight(), lj.getHeight()) / meanHeight;
  //                 scale = Math.max(minLineSizeScale, Math.min(scale, maxLineSizeScale));
  //                 // "<=" is used instead of "<" for consistency and to allow setting minVertical(Merge)Distance
  //                 // to 0.0 with meaning "no minimal distance required"
  //                 if (!sets.areTogether(li, lj) && li.angularDifference(lj) <= angleTolerance) {
  //                     double hDist = li.horizontalDistance(lj, orientation) / scale;
  //                     double vDist = li.verticalDistance(lj, orientation) / scale;
  //                     // Line over or above
  //                     if (minHorizontalDistance <= hDist && hDist <= maxHorizontalDistance
  //                             && minVerticalDistance <= vDist && vDist <= maxVerticalDistance) {
  //                         sets.union(li, lj);
  //                             }
  //                     // Split line that needs later merging
  //                     else if (minHorizontalMergeDistance <= hDist && hDist <= maxHorizontalMergeDistance
  //                             && minVerticalMergeDistance <= vDist && vDist <= maxVerticalMergeDistance) {
  //                         sets.union(li, lj);
  //                     }
  //                 }
  //             }
  //         }
  //         List[List[ComponentLine]] zones = new ArrayList[List[ComponentLine]]();
  //         for (Set[ComponentLine] group : sets) {
  //             zones.add(new ArrayList[ComponentLine](group));
  //         }
  //         return zones;
  //     }

  //     private List[List[ComponentLine]] mergeZones(List[List[ComponentLine]] zones, double tolerance) {
  //         List[BxBounds] bounds = new ArrayList[BxBounds](zones.size());
  //         for (List[ComponentLine] zone : zones) {
  //             BxBoundsBuilder builder = new BxBoundsBuilder();
  //             for (ComponentLine line : zone) {
  //                 for (Component component : line.getComponents()) {
  //                     builder.expand(component.getChunk().getBounds());
  //                 }
  //             }
  //             bounds.add(builder.getBounds());
  //         }

  //         List[List[ComponentLine]] outputZones = new ArrayList[List[ComponentLine]]();
  //         mainFor: for (int i = 0; i < zones.size(); i++) {
  //             for (int j = 0; j < zones.size(); j++) {
  //                 if (i == j || bounds.get(j) == null || bounds.get(i) == null) {
  //                     continue;
  //                 }
  //                 if (BxModelUtils.contains(bounds.get(j), bounds.get(i), tolerance)) {
  //                     zones.get(j).addAll(zones.get(i));
  //                     bounds.set(i, null);
  //                     continue mainFor;
  //                 }
  //             }
  //             outputZones.add(zones.get(i));
  //         }
  //         return outputZones;
  //     }

  //     private List[List[ComponentLine]] mergeLines(List[List[ComponentLine]] zones, double orientation,
  //             double minHorizontalDistance, double maxHorizontalDistance,
  //             double minVerticalDistance, double maxVerticalDistance) {
  //         List[List[ComponentLine]] outputZones = new ArrayList[List[ComponentLine]](zones.size());
  //         for (List[ComponentLine] zone : zones) {
  //             outputZones.add(mergeLinesInZone(zone, orientation,
  //                 minHorizontalDistance, maxHorizontalDistance,
  //                 minVerticalDistance, maxVerticalDistance));
  //         }
  //         return outputZones;
  //     }

  //     private List[ComponentLine] mergeLinesInZone(List[ComponentLine] lines, double orientation,
  //             double minHorizontalDistance, double maxHorizontalDistance,
  //             double minVerticalDistance, double maxVerticalDistance) {
  //         DisjointSets[ComponentLine] sets = new DisjointSets[ComponentLine](lines);
  //         for (int i = 0; i < lines.size(); i++) {
  //             ComponentLine li = lines.get(i);
  //             for (int j = i + 1; j < lines.size(); j++) {
  //                 ComponentLine lj = lines.get(j);
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
  //         List[ComponentLine] outputZone = new ArrayList[ComponentLine]();
  //         for (Set[ComponentLine] group : sets) {
  //             List[Component] components = new ArrayList[Component]();
  //             for (ComponentLine line : group) {
  //                 components.addAll(line.getComponents());
  //             }
  //             Collections.sort(components, ComponentXComparator.getInstance());
  //             outputZone.add(new ComponentLine(components, orientation));
  //         }
  //         return outputZone;
  //     }

  //     /**
  //      * Converts list of zones from internal format (using components and
  //      * component lines) to BxModel.
  //      *
  //      * @param zones zones in internal format
  //      * @param wordSpacing - maximum allowed distance between components that
  //      * belong to one word
  //      * @return BxModel page
  //      */
  //     private BxPage convertToBxModel(BxPage origPage, List[List[ComponentLine]] zones, double wordSpacing) {
  //         BxPage page = new BxPage();
  //         List[BxPage] pages = Lists.newArrayList(origPage.getParent());
  //         int pageIndex = pages.indexOf(origPage);
  //         boolean groupped = false;
  //         if (zones.size() > MAX_ZONES_PER_PAGE && pageIndex >= PAGE_MARGIN
  //                 && pageIndex < pages.size() - PAGE_MARGIN) {
  //             List<ComponentLine] oneZone = new ArrayList[ComponentLine]();
  //             for (List[ComponentLine] zone : zones) {
  //                 oneZone.addAll(zone);
  //             }
  //             zones = new ArrayList[List[ComponentLine]]();
  //             zones.add(oneZone);
  //             groupped = true;
  //         }

  //         for (List[ComponentLine] lines : zones) {
  //             BxZone zone = new BxZone();
  //             if (groupped) {
  //                 zone.setLabel(BxZoneLabel.GEN_OTHER);
  //             }
  //             for (ComponentLine line : lines) {
  //                 zone.addLine(line.convertToBxLine(wordSpacing));
  //             }
  //             List[BxLine] zLines = Lists.newArrayList(zone);
  //             Collections.sort(zLines, new Comparator[BxLine]() {

  //                 @Override
  //                 public int compare(BxLine o1, BxLine o2) {
  //                     return Double.compare(o1.getBounds().getY(), o2.getBounds().getY());
  //                 }

  //             });
  //             zone.setLines(zLines);
  //             BxBoundsBuilder.setBounds(zone);
  //             page.addZone(zone);
  //         }
  //         BxModelUtils.sortZonesYX(page);
  //         BxBoundsBuilder.setBounds(page);
  //         return page;
  //     }



  //     private static final double DISTANCE_STEP = 16.0;

  //     public static final double DEFAULT_ANGLE_HIST_RES = Math.toRadians(0.5);

  //     public static final double DEFAULT_ANGLE_HIST_SMOOTH_LEN = 0.25 * Math.PI;

  //     public static final double DEFAULT_ANGLE_HIST_SMOOTH_STDDEV = 0.0625 * Math.PI;

  //     public static final double DEFAULT_SPACE_HIST_RES = 0.5;

  //     public static final double DEFAULT_SPACE_HIST_SMOOTH_LEN = 2.5;

  //     public static final double DEFAULT_SPACE_HIST_SMOOTH_STDDEV = 0.5;

  //     public static final double DEFAULT_MAX_VERT_COMP_DIST = 0.67;

  //     public static final double DEFAULT_MIN_LINE_SIZE_SCALE = 0.9;

  //     public static final double DEFAULT_MAX_LINE_SIZE_SCALE = 2.5;

  //     public static final double DEFAULT_MIN_HORIZONTAL_DIST = -0.5;

  //     public static final double DEFAULT_MIN_VERTICAL_DIST = 0.0;

  //     public static final double DEFAULT_MAX_VERTICAL_DIST = 1.2;

  //     public static final double DEFAULT_COMP_DIST_CHAR = 3.5;

  //     public static final double DEFAULT_WORD_DIST = 0.2;

  //     public static final double DEFAULT_MIN_HORIZONTAL_MERGE_DIST = -3.0;

  //     public static final double DEFAULT_MAX_VERTICAL_MERGE_DIST = 0.5;

  //     public static final double DEFAULT_ANGLE_TOLERANCE = Math.PI / 6;

  //     public static final int DEFAULT_NEIGHBOR_COUNT = 5;


  //     /**
  //      * Angle histogram resolution in radians per bin.
  //      */
  //     private double angleHistogramResolution = DEFAULT_ANGLE_HIST_RES;

  //     /**
  //      * Angle histogram smoothing window length in radians.
  //      * Length of angle histogram is equal to pi.
  //      */
  //     private double angleHistogramSmoothingWindowLength = DEFAULT_ANGLE_HIST_SMOOTH_LEN;

  //     /**
  //      * Angle histogram gaussian smoothing window standard deviation in radians.
  //      */
  //     private double angleHistogramSmoothingWindowStdDeviation = DEFAULT_ANGLE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Spacing histogram resolution per bin.
  //      */
  //     private double spacingHistogramResolution = DEFAULT_SPACE_HIST_RES;

  //     /**
  //      * Spacing histogram smoothing window length.
  //      */
  //     private double spacingHistogramSmoothingWindowLength = DEFAULT_SPACE_HIST_SMOOTH_LEN;

  //     /**
  //      * Spacing histogram gaussian smoothing window standard deviation.
  //      */
  //     private double spacingHistogramSmoothingWindowStdDeviation = DEFAULT_SPACE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Maximum vertical component distance multiplier used during line
  //      * determination.
  //      *
  //      * Maximum vertical distance between components (characters) that belong
  //      * to the same line is equal to the product of this value and estimated
  //      * between-line spacing.
  //      */
  //     private double maxVerticalComponentDistanceMultiplier = DEFAULT_MAX_VERT_COMP_DIST;

  //     /**
  //      * Minimum line size scale value.
  //      *
  //      * During zone determination (merging lines into zones) line height is
  //      * taken into account. To achieve this, line size scale is estimated and
  //      * limited to range [minLineSizeScale, maxLineSizeScale].
  //      */
  //     private double minLineSizeScale = DEFAULT_MIN_LINE_SIZE_SCALE;

  //     /**
  //      * Maximum line size scale value.
  //      *
  //      * See minLineSizeScale for more information.
  //      */
  //     private double maxLineSizeScale = DEFAULT_MAX_LINE_SIZE_SCALE;

  //     /**
  //      * Minimum horizontal line distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated within-line spacing.
  //      */
  //     private double minHorizontalDistanceMultiplier = DEFAULT_MIN_HORIZONTAL_DIST;

  //     /**
  //      * Minimum vertical line distance multiplier.
  //      *
  //      * Minimum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
  //     private double minVerticalDistanceMultiplier = DEFAULT_MIN_VERTICAL_DIST;

  //     /**
  //      * Maximum vertical line distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
  //     private double maxVerticalDistanceMultiplier = DEFAULT_MAX_VERTICAL_DIST;

  //     /**
  //      * Component distance character spacing multiplier.
  //      *
  //      * Maximum distance between components that belong to the same line is
  //      * equal to (lineSpacing * componentDistanceLineMultiplier +
  //      * characterSpacing * componentDistanceCharacterMultiplier), where
  //      * lineSpacing and characterSpacing are estimated between-line and
  //      * within-line spacing, respectively.
  //      */
  //     private double componentDistanceCharacterMultiplier = DEFAULT_COMP_DIST_CHAR;

  //     /**
  //      * Word distance multiplier.
  //      *
  //      * Maximum distance between components that belong to the same word is
  //      * equal to the product of this value and estimated within-line spacing.
  //      */
  //     private double wordDistanceMultiplier = DEFAULT_WORD_DIST;

  //     /**
  //      * Minimum horizontal line merge distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that should be merged is equal
  //      * to the product of this value and estimated within-line spacing.
  //      *
  //      * Because split lines do not overlap this value should be negative.
  //      */

  //     private double minHorizontalMergeDistanceMultiplier = DEFAULT_MIN_HORIZONTAL_MERGE_DIST;

  //     /**
  //      * Maximum vertical line merge distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that should be merged is equal
  //      * to the product of this value and estimated between-line spacing.
  //      */

  //     private double maxVerticalMergeDistanceMultiplier = DEFAULT_MAX_VERTICAL_MERGE_DIST;

  //     /**
  //      * Angle tolerance for comparisons of angles between components and angles
  //      * between lines.
  //      */
  //     private double angleTolerance = DEFAULT_ANGLE_TOLERANCE;

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
