package edu.umass.cs.iesl.watr
package segment

/**
  * Filter class for neighbor objects that checks if the angle of the
  * neighbor is within specified range.
  */
object AngleFilter {
  def apply(lowerAngle: Double, upperAngle: Double): AngleFilter = {
    val low = if (lowerAngle < -Math.PI/2) {
      lowerAngle + Math.PI;
    } else lowerAngle

    val hi = if (upperAngle >= Math.PI/2) {
      upperAngle - Math.PI;
    } else upperAngle

    if (low <= hi) {
      new AndFilter(low, hi);
    } else {
      new OrFilter(low, hi);
    }

  }
}


trait AngleFilter {
  def lowerAngle: Double
  def upperAngle: Double

  def matches(angle: Double): Boolean
}

class AndFilter(
  override val lowerAngle: Double,
  override val upperAngle: Double
) extends AngleFilter {


  def matches(angle: Double): Boolean = {
    // val nangle = n.bbox.toCenterPoint.angleTo(origin.bbox.toCenterPoint)
    lowerAngle <= angle && angle < upperAngle
  }
}

class OrFilter(
  override val lowerAngle: Double,
  override val upperAngle: Double
) extends AngleFilter {


  def matches(angle: Double): Boolean = {
    // val nangle = n.bbox.toCenterPoint.angleTo(origin.bbox.toCenterPoint)
    lowerAngle <= angle || angle < upperAngle
  }

}


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





