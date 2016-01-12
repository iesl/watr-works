package edu.umass.cs.iesl.watr
package watrcolors
package models

case class Rectangle(
  x: Double,
  y: Double,
  width: Double,
  height: Double
)

case class Annotation(
  label: String,
  rect: List[Rectangle]
)

case class Document (
  fileName: String,
  annotations: List[Annotation]
){

    override
    def toString()  = {
        "Document{" +
                "fileName='" + fileName + '\'' +
                "annotations='" + annotations + '\'' +
                '}';
    }

    def addAnnotations(annotations: List[Annotation]) {
        for (annotation <- annotations) {
            addAnnotation(annotation);
        }
    }

    def addAnnotation(Annotation annotation) {
        annotations.add(annotation);
    }

    def getAnnotations: List[Annotation] = {
        return annotations;
    }

    def clearAnnotations() {
      annotations.clear()
    }

  def serializeAnnotationsToJson() {
         Json.toJson(annotations);
    }

    /**
     * @param annotationsJsonNode a JsonNode as passed to Application.saveDocument()
     * @return
     */
    def deserializeAnnotationListFromJson(JsonNode annotationsJsonNode): List[Annotation] = {
        val newAnnotations = List[Annotation]()

        for ( annotNode <- annotationsJsonNode) {        // {"label":"title", "rects":[{"x":100, "y":100, "width":400, "height":50}]}
            val label = annotNode.findValue("label").asText();
            val rects = List[Rect]()
            for (rectNode <- annotNode.findValue("rects")) {
                val x = rectNode.get("x").asDouble();
                val y = rectNode.get("y").asDouble();
                val width = rectNode.get("width").asDouble();
                val height = rectNode.get("height").asDouble();
                rects.add(new Rectangle(x, y, width, height));
            }
            newAnnotations.add(new Annotation(label, rects));
        }
        return newAnnotations;
    }

}
