package models;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * An annotated rectangle.
 */
public class Annotation {

    public String label;
    public List<Rectangle> rects;

    /**
     * @param label
     * @param rects NB: does *not* copy rects - uses passed arg directly
     */
    public Annotation(String label, List<Rectangle> rects) {
        if ((label == null) || (rects == null)) {
            throw new IllegalArgumentException("label and rects must be non-null");
        }

        this.label = label;
        this.rects = rects;
    }

    @Override
    public String toString() {
        return "Annotation{" +
                "label='" + label + '\'' +
                ", rects=" + rects +
                '}';
    }

    @Override
    public int hashCode() {
        return label.hashCode() + rects.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Annotation)) {
            return false;
        }

        Annotation otherAnnot = (Annotation) obj;
        if (!this.label.equals(otherAnnot.label)) {
            return false;
        } else if (otherAnnot.rects.size() != otherAnnot.rects.size()) {
            return false;
        } else {
            // zip lists and compare item-by-item. NB: ASSUMES RECTS IN SAME ORDER!. if undesirable, use something like http://stackoverflow.com/questions/13501142/java-arraylist-how-can-i-tell-if-two-lists-are-equal-order-not-mattering
            for (int idx = 0; idx < rects.size(); idx++) {
                if (!(rects.get(idx).equals(otherAnnot.rects.get(idx)))) {
                    return false;
                }
            }

            return true;
        }
    }

}