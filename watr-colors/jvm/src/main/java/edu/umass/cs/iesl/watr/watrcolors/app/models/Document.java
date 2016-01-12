package models;

import com.fasterxml.jackson.databind.JsonNode;
import play.libs.Json;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents an annotated SVG file.
 */
public class Document {

    public String fileName;     // TODO getter
    private List<Annotation> annotations = new ArrayList<>();

    public Document(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public String toString() {
        return "Document{" +
                "fileName='" + fileName + '\'' +
                "annotations='" + annotations + '\'' +
                '}';
    }

    public void addAnnotations(List<Annotation> annotations) {
        for (Annotation annotation : annotations) {
            addAnnotation(annotation);
        }
    }

    public void addAnnotation(Annotation annotation) {
        annotations.add(annotation);
    }

    public List<Annotation> getAnnotations() {
        return annotations;
    }

    public void clearAnnotations() {
        annotations.clear();
    }

    public JsonNode serializeAnnotationsToJson() {
        return Json.toJson(annotations);
    }

    /**
     * @param annotationsJsonNode a JsonNode as passed to Application.saveDocument()
     * @return
     */
    public static List<Annotation> deserializeAnnotationListFromJson(JsonNode annotationsJsonNode) {
        List<Annotation> newAnnotations = new ArrayList<>();
        for (JsonNode annotNode : annotationsJsonNode) {        // {"label":"title", "rects":[{"x":100, "y":100, "width":400, "height":50}]}
            String label = annotNode.findValue("label").asText();
            List<Rectangle> rects = new ArrayList<>();
            for (JsonNode rectNode : annotNode.findValue("rects")) {
                double x = rectNode.get("x").asDouble();
                double y = rectNode.get("y").asDouble();
                double width = rectNode.get("width").asDouble();
                double height = rectNode.get("height").asDouble();
                rects.add(new Rectangle(x, y, width, height));
            }
            newAnnotations.add(new Annotation(label, rects));
        }
        return newAnnotations;
    }

}
