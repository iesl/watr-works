package edu.umass.cs.iesl.watr
package ext


import java.io.InputStream
import org.jdom2.Element


import pl.edu.icm.cermine.structure.ITextCharacterExtractor
import pl.edu.icm.cermine.structure.model.BxDocument

class CermineExtractor extends ITextCharacterExtractor {

  // def  renderText(TextRenderInfo tri) {

  def  transferTspan(tspan: Element): Unit = {
    // for (TextRenderInfo charTri : tri.getCharacterRenderInfos()) {
    //   String text = charTri.getText();
    //   char ch = charTri.getText().charAt(0);
    //   if (ch <= ' ' || text.matches("^[\uD800-\uD8FF]$")
    //           || text.matches("^[\uDC00-\uDFFF]$")
    //           || text.matches("^[\uFFF0-\uFFFF]$")) {
    //       continue;
    //   }

    //   float absoluteCharLeft = charTri.getDescentLine().getStartPoint().get(Vector.I1);
    //   float absoluteCharBottom = charTri.getDescentLine().getStartPoint().get(Vector.I2);

    //   float charLeft = absoluteCharLeft - pageRectangle.getLeft();
    //   float charBottom = absoluteCharBottom - pageRectangle.getBottom();

    //   float charHeight = charTri.getAscentLine().getStartPoint().get(Vector.I2)
    //           - charTri.getDescentLine().getStartPoint().get(Vector.I2);
    //   float charWidth = charTri.getDescentLine().getLength();

    //   if (Float.isNaN(charHeight) || Float.isInfinite(charHeight)) {
    //       charHeight = 0;
    //   }

    //   if (Float.isNaN(charWidth) || Float.isInfinite(charWidth)) {
    //       charWidth = 0;
    //   }

    //   if (absoluteCharLeft < pageRectangle.getLeft()
    //           || absoluteCharLeft + charWidth > pageRectangle.getRight()
    //           || absoluteCharBottom < pageRectangle.getBottom()
    //           || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
    //       continue;
    //   }

    //   BxBounds bounds = new BxBounds(charLeft, pageRectangle.getHeight() - charBottom - charHeight,
    //                                  charWidth, charHeight);

    //   if (Double.isNaN(bounds.getX()) || Double.isInfinite(bounds.getX())
    //           || Double.isNaN(bounds.getY()) || Double.isInfinite(bounds.getY())
    //           || Double.isNaN(bounds.getHeight()) || Double.isInfinite(bounds.getHeight())
    //           || Double.isNaN(bounds.getWidth()) || Double.isInfinite(bounds.getWidth())) {
    //       continue;
    //   }

    //   BxChunk chunk = new BxChunk(bounds, text);
    //   chunk.setFontName(tri.getFont().getFullFontName()[0][3]);
    //   actPage.addChunk(chunk);
    //   boundsBuilder.expand(bounds);
    // }
  }




  // val in = new FileInputStream(svgIS);

  override def extractCharacters(svgIS: InputStream):BxDocument = {
    val bxDocument = new BxDocument()


    bxDocument
  }

}
