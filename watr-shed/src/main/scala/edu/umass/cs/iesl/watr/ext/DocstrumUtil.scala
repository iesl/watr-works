// package edu.umass.cs.iesl.watr
// package ext

// import watrmarks._

// import com.itextpdf.text.Rectangle
// import com.itextpdf.text.exceptions.InvalidPdfException
// import com.itextpdf.text.pdf._
// import com.itextpdf.text.pdf.parser.{Vector => PVector, RenderListener, _}
//   // import pl.edu.icm.cermine.exception.AnalysisException
//   // import pl.edu.icm.cermine.structure.CharacterExtractor
// import _root_.pl.edu.icm.cermine.structure.model._

// // import pl.edu.icm.cermine.structure.model.BxPage
// // import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder

//     /**
//      * Internal representation of character.
//      */
//     protected static class Component {

//         private final double x;
//         private final double y;
//         private final BxChunk chunk;

//         private List<Neighbor> neighbors;

//         public Component(BxChunk chunk) {
//             BxBounds bounds = chunk.getBounds();
//             if (bounds == null) {
//                 throw new IllegalArgumentException("Bounds must not be null");
//             }
//             if (Double.isNaN(bounds.getX()) || Double.isInfinite(bounds.getX())) {
//                 throw new IllegalArgumentException("Bounds x coordinate must be finite");
//             }
//             if (Double.isNaN(bounds.getY()) || Double.isInfinite(bounds.getY())) {
//                 throw new IllegalArgumentException("Bounds y coordinate must be finite");
//             }
//             if (Double.isNaN(bounds.getWidth()) || Double.isInfinite(bounds.getWidth())) {
//                 throw new IllegalArgumentException("Bounds width must be finite");
//             }
//             if (Double.isNaN(bounds.getHeight()) || Double.isInfinite(bounds.getHeight())) {
//                 throw new IllegalArgumentException("Bounds height must be finite");
//             }
//             this.x = chunk.getBounds().getX() + chunk.getBounds().getWidth() / 2;
//             this.y = chunk.getBounds().getY() + chunk.getBounds().getHeight() / 2;
//             this.chunk = chunk;
//         }

//         public double getX() {
//             return x;
//         }

//         public double getY() {
//             return y;
//         }

//         public double getHeight() {
//             return chunk.getBounds().getHeight();
//         }

//         public double distance(Component c) {
//             double dx = getX() - c.getX(), dy = getY() - c.getY();
//             return Math.sqrt(dx * dx + dy * dy);
//         }

//         /**
//          * Computes horizontal distance between components.
//          *
//          * @param c
//          * @param orientation
//          * @return
//          */
//         public double horizontalDistance(Component c, double orientation) {
//             // TODO: take orientation into account
//             return Math.abs(getX() - c.getX());
//         }

//         public double verticalDistance(Component c, double orientation) {
//             return Math.abs(getY() - c.getY());
//         }

//         public double horizontalBoundsDistance(Component c, double orientation) {
//             // TODO: take orientation into account
//             return horizontalDistance(c, orientation) - getChunk().getBounds().getWidth() / 2 -
//                     c.getChunk().getBounds().getWidth() / 2;
//         }


//         public BxChunk getChunk() {
//             return chunk;
//        }

//         public List<Neighbor> getNeighbors() {
//             return neighbors;
//         }

//         public void setNeighbors(List<Neighbor> neighbors) {
//             this.neighbors = neighbors;
//         }

//         private double angle(Component c) {
//             if (getX() > c.getX()) {
//                 return Math.atan2(getY() - c.getY(), getX() - c.getX());
//             } else {
//                 return Math.atan2(c.getY() - getY(), c.getX() - getX());
//             }
//         }

//         public double overlappingDistance(Component other, double orientation) {
//             double[] xs = new double[4];
//             double s = Math.sin(-orientation), c = Math.cos(-orientation);
//             xs[0] = c * x - s * y;
//             xs[1] = c * (x+chunk.getWidth()) - s * (y+chunk.getHeight());
//             xs[2] = c * other.x - s * other.y;
//             xs[3] = c * (other.x+other.chunk.getWidth()) - s * (other.y+other.chunk.getHeight());
//             boolean overlapping = xs[1] >= xs[2] && xs[3] >= xs[0];
//             Arrays.sort(xs);
//             return Math.abs(xs[2] - xs[1]) * (overlapping ? 1 : -1);
//         }
//     }
//     /**
//      * Class representing nearest-neighbor pair.
//      */
//     protected static class Neighbor {

//         private final double distance;
//         private final double angle;
//         private final Component component;
//         private final Component origin;

//         public Neighbor(Component neighbor, Component origin) {
//             this.distance = neighbor.distance(origin);
//             this.angle = neighbor.angle(origin);
//             this.component = neighbor;
//             this.origin = origin;
//         }

//         public double getDistance() {
//             return distance;
//         }

//         public double getHorizontalDistance(double orientation) {
//             return component.horizontalDistance(origin, orientation);
//         }

//         public double getVerticalDistance(double orientation) {
//             return component.verticalDistance(origin, orientation);
//         }

//         public double getAngle() {
//             return angle;
//         }

//         public Component getComponent() {
//             return component;
//         }
//     }

//     /**
//      * Component comparator based on x coordinate of the centroid of component.
//      *
//      * The ordering is not consistent with equals.
//      */
//     protected static final class ComponentXComparator implements Comparator<Component> {

//         private ComponentXComparator() {
//         }

//         @Override
//         public int compare(Component o1, Component o2) {
//             return Double.compare(o1.getX(), o2.getX());
//         }

//         private static ComponentXComparator instance = new ComponentXComparator();

//         public static ComponentXComparator getInstance() {
//             return instance;
//         }
//     }

//     /**
//      * Neighbor distance comparator based on the distance.
//      *
//      * The ordering is not consistent with equals.
//      */
//     protected static final class NeighborDistanceComparator implements Comparator<Neighbor> {

//         private NeighborDistanceComparator() {
//         }

//         @Override
//         public int compare(Neighbor o1, Neighbor o2) {
//             return Double.compare(o1.getDistance(), o2.getDistance());
//         }

//         private static NeighborDistanceComparator instance = new NeighborDistanceComparator();

//         public static NeighborDistanceComparator getInstance() {
//             return instance;
//         }
//     }

//     /**
//      * Internal representation of the text line.
//      */
//     protected static class ComponentLine {

//         private final double x0;
//         private final double y0;

//         private final double x1;
//         private final double y1;

//         private final double height;

//         private List<Component> components;

//         public ComponentLine(List<Component> components, double orientation) {
//             this.components = components;

//             if (components.size() >= 2) {
//                 // Simple linear regression
//                 double sx = 0.0, sxx = 0.0, sxy = 0.0, sy = 0.0;
//                 for (Component component : components) {
//                     sx += component.getX();
//                     sxx += component.getX() * component.getX();
//                     sxy += component.getX() * component.getY();
//                     sy += component.getY();
//                 }
//                 double b = (components.size() * sxy - sx * sy) / (components.size() * sxx - sx * sx);
//                 double a = (sy - b * sx) / components.size();

//                 this.x0 = components.get(0).getX();
//                 this.y0 = a + b * this.x0;
//                 this.x1 = components.get(components.size() - 1).getX();
//                 this.y1 = a + b * this.x1;
//             }
//             else if (! components.isEmpty()) {
//                 Component component = components.get(0);
//                 double dx = component.getChunk().getBounds().getWidth() / 3;
//                 double dy = dx * Math.tan(orientation);
//                 this.x0 = component.getX() - dx;
//                 this.x1 = component.getX() + dx;
//                 this.y0 = component.getY() - dy;
//                 this.y1 = component.getY() + dy;
//             }
//             else {
//                 throw new IllegalArgumentException("Component list must not be empty");
//             }
//             height = computeHeight();
//         }

//         public double getAngle() {
//             return Math.atan2(y1 - y0, x1 - x0);
//         }

//         public double getSlope() {
//             return (y1 - y0) / (x1 - x0);
//         }

//         public double getLength() {
//             return Math.sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1));
//         }

//         private double computeHeight() {
//             double sum = 0.0;
//             for (Component component : components) {
//                 sum += component.getHeight();
//             }
//             return sum / components.size();
//         }

//         public double getHeight() {
//             return height;
//         }

//         public List<Component> getComponents() {
//             return components;
//         }

//         public double angularDifference(ComponentLine j) {
//             double diff = Math.abs(getAngle() - j.getAngle());
//             if (diff <= Math.PI/2) {
//                 return diff;
//             } else {
//                 return Math.PI - diff;
//             }
//         }

//         public double horizontalDistance(ComponentLine other, double orientation) {
//             double[] xs = new double[4];
//             double s = Math.sin(-orientation), c = Math.cos(-orientation);
//             xs[0] = c * x0 - s * y0;
//             xs[1] = c * x1 - s * y1;
//             xs[2] = c * other.x0 - s * other.y0;
//             xs[3] = c * other.x1 - s * other.y1;
//             boolean overlapping = xs[1] >= xs[2] && xs[3] >= xs[0];
//             Arrays.sort(xs);
//             return Math.abs(xs[2] - xs[1]) * (overlapping ? 1 : -1);
//         }

//         public double verticalDistance(ComponentLine other, double orientation) {
//             double xm = (x0 + x1) / 2, ym = (y0 + y1) / 2;
//             double xn = (other.x0 + other.x1) / 2, yn = (other.y0 + other.y1) / 2;
//             double a = Math.tan(orientation);
//             return Math.abs(a * (xn - xm) + ym - yn) / Math.sqrt(a * a + 1);
//         }

//         public BxLine convertToBxLine(double wordSpacing) {
//             BxLine line = new BxLine();
//             BxWord word = new BxWord();
//             Component previousComponent = null;
//             for (Component component : components) {
//                 if (previousComponent != null) {
//                     double dist = component.getChunk().getBounds().getX() -
//                             previousComponent.getChunk().getBounds().getX() -
//                             previousComponent.getChunk().getBounds().getWidth();
//                     if(dist > wordSpacing) {
//                         BxBoundsBuilder.setBounds(word);
//                         line.addWord(word);
//                         word = new BxWord();
//                     }
//                 }
//                 word.addChunk(component.getChunk());
//                 previousComponent = component;
//             }
//             BxBoundsBuilder.setBounds(word);
//             line.addWord(word);
//             BxBoundsBuilder.setBounds(line);
//             return line;
//         }
//     }

//     /**
//      * Filter class for neighbor objects that checks if the angle of the
//      * neighbor is within specified range.
//      */
//     protected abstract static class AngleFilter {

//         private final double lowerAngle;
//         private final double upperAngle;

//         private AngleFilter(double lowerAngle, double upperAngle) {
//             this.lowerAngle = lowerAngle;
//             this.upperAngle = upperAngle;
//         }

//         /**
//          * Constructs new angle filter.
//          *
//          * @param lowerAngle minimum angle in range [-3*pi/2, pi/2)
//          * @param upperAngle maximum angle in range [-pi/2, 3*pi/2)
//          * @return newly constructed angle filter
//          */
//         public static AngleFilter newInstance(double lowerAngle, double upperAngle) {
//             if (lowerAngle < -Math.PI/2) {
//                 lowerAngle += Math.PI;
//             }
//             if (upperAngle >= Math.PI/2) {
//                 upperAngle -= Math.PI;
//             }
//             if (lowerAngle <= upperAngle) {
//                 return new AndFilter(lowerAngle, upperAngle);
//             } else {
//                 return new OrFilter(lowerAngle, upperAngle);
//             }
//         }

//         public double getLowerAngle() {
//             return lowerAngle;
//         }

//         public double getUpperAngle() {
//             return upperAngle;
//         }

//         public abstract boolean matches(Neighbor neighbor);

//         public static final class AndFilter extends AngleFilter {

//             private AndFilter(double lowerAngle, double upperAngle) {
//                 super(lowerAngle, upperAngle);
//             }

//             @Override
//             public boolean matches(Neighbor neighbor) {
//                 return getLowerAngle() <= neighbor.getAngle() && neighbor.getAngle() < getUpperAngle();
//             }

//         }

//         public static final class OrFilter extends AngleFilter {

//             private OrFilter(double lowerAngle, double upperAngle) {
//                 super(lowerAngle, upperAngle);
//             }

//             @Override
//             public boolean matches(Neighbor neighbor) {
//                 return getLowerAngle() <= neighbor.getAngle() || neighbor.getAngle() < getUpperAngle();
//             }

//         }
//     }





























