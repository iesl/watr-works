package models;

public class Rectangle {

    public double x;
    public double y;
    public double width;
    public double height;

    public Rectangle(double x, double y, double width, double height) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    @Override
    public String toString() {
        return "Rectangle{" +
                "x=" + x +
                ", y=" + y +
                ", width=" + width +
                ", height=" + height +
                '}';
    }

    @Override
    public int hashCode() {
        return (int) (x + y + width + height);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Rectangle)) {
            return false;
        }

        Rectangle otherRect = (Rectangle) obj;
        return (this.x == otherRect.x) && (this.y == otherRect.y) &&
                (this.width == otherRect.width) && (this.height == otherRect.height);
    }

}
