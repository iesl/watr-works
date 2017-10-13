package edu.umass.cs.iesl.watr
package extract

import org.apache.pdfbox.pdmodel.font.PDFont
import org.apache.pdfbox.util.{ Matrix, Vector }
import utils.EnrichNumerics._

import org.apache.pdfbox.text._
import org.apache.pdfbox.contentstream._
import java.io._
import java.text.Bidi
import java.text.Normalizer
  // import java.util._
import java.util.regex.Pattern

// import org.apache.commons.logging.Log;
// import org.apache.commons.logging.LogFactory;
import org.apache.pdfbox.pdmodel._;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
// import org.apache.pdfbox.pdmodel.interactive.pagenavigation.PDThreadBead;
import org.apache.pdfbox.util.QuickSort;
import scala.collection.immutable.{List => _}


// import scala.collection.mutable
import scala.collection.JavaConverters._

// import java.util.Collections
import java.util.TreeMap
import java.util.TreeSet
import java.util.HashMap
import java.util.ArrayList
import java.{util => ju}
import java.lang.{StringBuilder}

class PdfBoxExtractor() extends PDFStreamEngine {
  // // protected def document_=(x$1: org.apache.pdfbox.pdmodel.PDDocument): Unit = ???
  // // protected def output_=(x$1: java.io.Writer): Unit = ???



  private var _document: PDDocument = null
  private var _output: Writer = null

  protected def document: PDDocument = _document
  protected def output: Writer = _output

  private def defaultIndentThreshold = 2.0f;
  private def defaultDropThreshold = 2.5f;
  private def useCustomQuickSort = true;

  // private static final Log LOG = LogFactory.getLog(PDFTextStripper.class);


  protected var LINE_SEPARATOR = System.getProperty("line.separator");

  private var lineSeparator = LINE_SEPARATOR;
  private var wordSeparator = " ";
  private var paragraphStart = "";
  private var paragraphEnd = "";
  private var pageStart = "";
  private var pageEnd = LINE_SEPARATOR;
  private var articleStart = "";
  private var articleEnd = "";

  private var currentPageNo = 0;
  private var startPageNum = 1;
  private var endPage = Integer.MAX_VALUE;
  private var startBookmark: PDOutlineItem = null;

  // 1-based bookmark pages
  private var startBookmarkPageNumber = -1;
  private var endBookmarkPageNumber = -1;

  private var endBookmark: PDOutlineItem = null;
  private var suppressDuplicateOverlappingText = true;
  private var shouldSeparateByBeads = true;
  private var sortByPosition = false;
  private var addMoreFormatting = false;

  private var indentThreshold = defaultIndentThreshold;
  private var dropThreshold = defaultDropThreshold;

  // we will need to estimate where to add spaces, these are used to help guess
  private var spacingTolerance = .5f;
  private var averageCharTolerance = .3f;

  private var beadRectangles: ju.List[PDRectangle] = null


  /**
    * The charactersByArticle is used to extract text by article divisions. For example a PDF that has two columns like
    * a newspaper, we want to extract the first column and then the second column. In this example the PDF would have 2
    * beads(or articles), one for each column. The size of the charactersByArticle would be 5, because not all text on
    * the screen will fall into one of the articles. The five divisions are shown below
    *
    * Text before first article
    * first article text
    * text between first article and second article
    * second article text
    * text after second article
    *
    * Most PDFs won't have any beads, so charactersByArticle will contain a single entry.
    */



  // val charactersByArticle = mutable.ArrayBuffer[List[TextPosition]] ()
  val charactersByArticle: ArrayList[ju.List[TextPosition]]  = new ArrayList[ju.List[TextPosition]]();
  val characterListMapping: HashMap[String, TreeMap[Float, TreeSet[Float]]]  = new HashMap[String, TreeMap[Float, TreeSet[Float]]]();


  /**
    * True if we started a paragraph but haven't ended it yet.
    */
  private var inParagraph: Boolean = false;



  def getText(doc: PDDocument): String = {
    val outputStream = new StringWriter();
    writeText(doc, outputStream);
    outputStream.toString();
  }

  def resetEngine(): Unit = {
    currentPageNo = 0;
    _document = null;
    if (charactersByArticle != null) {
      charactersByArticle.clear();
    }

    if (characterListMapping != null) {
      characterListMapping.clear();
    }
  }

  /**
    * This will take a PDDocument and write the text of that document to the print writer.
    *
    * @param doc The document to get the data from.
    * @param outputStream The location to put the text.
    *
    * @throws IOException If the doc is in an invalid state.
    */
  def writeText(doc: PDDocument,  outputStream: Writer): Unit = {
    resetEngine();
    _document = doc;
    _output = outputStream;
    if (getAddMoreFormatting()) {
      paragraphEnd = lineSeparator;
      pageStart = lineSeparator;
      articleStart = lineSeparator;
      articleEnd = lineSeparator;
    }
    startDocument(document);
    processPages(document.getPages());
    endDocument(document);
  }
  /**
    * This will process all of the pages and the text that is in them.
    *
    * @param pages The pages object in the document.
    *
    * @throws IOException If there is an error parsing the text.
    */
  def  processPages(pages: PDPageTree): Unit = {
    val startBookmarkPage: PDPage = if (startBookmark == null)  null
                                    else startBookmark.findDestinationPage(document);
    if (startBookmarkPage != null) {
      startBookmarkPageNumber = pages.indexOf(startBookmarkPage) + 1;
    } else {
      // -1 = undefined
      startBookmarkPageNumber = -1;
    }

    val endBookmarkPage: PDPage = if (endBookmark == null)  null
                                  else endBookmark.findDestinationPage(document);
    if (endBookmarkPage != null) {
      endBookmarkPageNumber = pages.indexOf(endBookmarkPage) + 1;
    } else {
      // -1 = undefined
      endBookmarkPageNumber = -1;
    }

    if (startBookmarkPageNumber == -1 && startBookmark != null && endBookmarkPageNumber == -1
      && endBookmark != null
      && startBookmark.getCOSObject() == endBookmark.getCOSObject())
    {
      // this is a special case where both the start and end bookmark
      // are the same but point to nothing. In this case
      // we will not extract any text.
      startBookmarkPageNumber = 0;
      endBookmarkPageNumber = 0;
    }

    for ( page <- pages.iterator().asScala) {
      println(s"Proc. Pg ${page}")
      currentPageNo += 1;
      if (page.hasContents()) {
        processPage(page);
      }
    }
  }

  /**
    * This method is available for subclasses of this class. It will be called before processing of the document start.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  def  startDocument( document: PDDocument) : Unit = {
    println(s"startDocument")
  }

  /**
    * This method is available for subclasses of this class. It will be called after processing of the document
    * finishes.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  def  endDocument(document: PDDocument) : Unit = {
    println(s"endDocument")
  }

  /**
    * This will process the contents of a page.
    *
    * @param page The page to process.
    *
    * @throws IOException If there is an error processing the page.
    */
  override def  processPage( page: PDPage) : Unit = {
    if (currentPageNo >= startPageNum && currentPageNo <= endPage
      && (startBookmarkPageNumber == -1 || currentPageNo >= startBookmarkPageNumber)
      && (endBookmarkPageNumber == -1 || currentPageNo <= endBookmarkPageNumber)) {

      startPage(page);

      var numberOfArticleSections : Int = 1;

      if (shouldSeparateByBeads) {
        fillBeadRectangles(page);
        numberOfArticleSections = numberOfArticleSections + beadRectangles.size() * 2;
      }

      val originalSize : Int = charactersByArticle.size();
      charactersByArticle.ensureCapacity(numberOfArticleSections);
      val lastIndex : Int = math.max(numberOfArticleSections, originalSize);

      for (i <- 0 until lastIndex) {
        if (i < originalSize) {
          charactersByArticle.get(i).clear();
        } else {
          if (numberOfArticleSections < originalSize) {
            charactersByArticle.remove(i);
          } else {
            charactersByArticle.add(new ArrayList[TextPosition]());
          }
        }
      }
      characterListMapping.clear();
      super.processPage(page);
      writePage();
      endPage(page);
    }
  }

  def  fillBeadRectangles( page: PDPage): Unit = {
    beadRectangles = new ArrayList[PDRectangle]();
    for (bead <- page.getThreadBeads().iterator().asScala) {
      if (bead == null) {
        // can't skip, because of null entry handling in processTextPosition()
        beadRectangles.add(null);
      } else {


        val rect : PDRectangle= bead.getRectangle();

        // bead rectangle is in PDF coordinates (y=0 is bottom),
        // glyphs are in image coordinates (y=0 is top),
        // so we must flip
        val mediaBox : PDRectangle= page.getMediaBox();
        val upperRightY : Float= mediaBox.getUpperRightY() - rect.getLowerLeftY();
        val lowerLeftY : Float= mediaBox.getUpperRightY() - rect.getUpperRightY();
        rect.setLowerLeftY(lowerLeftY);
        rect.setUpperRightY(upperRightY);

        // adjust for cropbox
        val cropBox : PDRectangle= page.getCropBox();
        if (cropBox.getLowerLeftX() != 0 || cropBox.getLowerLeftY() != 0) {
          rect.setLowerLeftX(rect.getLowerLeftX() - cropBox.getLowerLeftX());
          rect.setLowerLeftY(rect.getLowerLeftY() - cropBox.getLowerLeftY());
          rect.setUpperRightX(rect.getUpperRightX() - cropBox.getLowerLeftX());
          rect.setUpperRightY(rect.getUpperRightY() - cropBox.getLowerLeftY());
        }

        beadRectangles.add(rect);
      }
    }
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead). This
    * assumes that the primary direction of text is left to right. Default implementation is to do nothing. Subclasses
    * may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  def  startArticle() : Unit = {
    startArticle(true);
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead).
    * Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param isLTR true if primary direction of text is left to right.
    * @throws IOException If there is any error writing to the stream.
    */
  def  startArticle( isLTR: Boolean) : Unit = {
    output.write(articleStart);
  }

  /**
    * End an article. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  def  endArticle() : Unit = {
    output.write(articleEnd);
  }

  /**
    * Start a new page. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param page The page we are about to process.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  def startPage( page: PDPage) : Unit = {
    println(s"   Start page ${page}")
  }

  /**
    * End a page. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param page The page we are about to process.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  def  endPage( page: PDPage) : Unit = {
    // default is to do nothing
  }

  private val END_OF_LAST_TEXT_X_RESET_VALUE :Float = -1;
  private val MAX_Y_FOR_LINE_RESET_VALUE :Float = -Float.MaxValue;
  private val EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE :Float = -Float.MaxValue;
  private val MAX_HEIGHT_FOR_LINE_RESET_VALUE :Float = -1;
  private val MIN_Y_TOP_FOR_LINE_RESET_VALUE :Float = Float.MaxValue
  private val LAST_WORD_SPACING_RESET_VALUE :Float = -1;

  /**
    * This will print the text of the processed page to "output". It will estimate, based on the coordinates of the
    * text, where newlines and word spacings should be placed. The text will be sorted only if that feature was
    * enabled.
    *
    * @throws IOException If there is an error writing the text.
    */
  def  writePage() : Unit = {
    var  maxYForLine: Float = MAX_Y_FOR_LINE_RESET_VALUE;
    var  minYTopForLine :Float = MIN_Y_TOP_FOR_LINE_RESET_VALUE;
    var  endOfLastTextX :Float = END_OF_LAST_TEXT_X_RESET_VALUE;
    var  lastWordSpacing :Float = LAST_WORD_SPACING_RESET_VALUE;
    var  maxHeightForLine :Float = MAX_HEIGHT_FOR_LINE_RESET_VALUE;
    var lastPosition : PositionWrapper = null;
    var lastLineStartPosition : PositionWrapper= null;

    var startOfPage : Boolean= true; // flag to indicate start of page
    var startOfArticle: Boolean = false;

    if (charactersByArticle.size() > 0) {
      writePageStart();
    }

    // for (List[TextPosition] textList : charactersByArticle) {
    for ( textList <- charactersByArticle.iterator().asScala) {
      if (getSortByPosition()) {
        val comparator : TextPositionComparator= new TextPositionComparator();

        // because the TextPositionComparator is not transitive, but
        // JDK7+ enforces transitivity on comparators, we need to use
        // a custom quicksort implementation (which is slower, unfortunately).
        if (useCustomQuickSort)
        {
          QuickSort.sort(textList, comparator);
        }
        else
        {
          ju.Collections.sort(textList, comparator);
        }
      }

      startArticle();
      startOfArticle = true;

      // Now cycle through to print the text.
      // We queue up a line at a time before we print so that we can convert
      // the line from presentation form to logical form (if needed).
      val line = new ArrayList[LineItem]();

      val textIter = textList.iterator();
      // PDF files don't always store spaces. We will need to guess where we should add
      // spaces based on the distances between TextPositions. Historically, this was done
      // based on the size of the space character provided by the font. In general, this
      // worked but there were cases where it did not work. Calculating the average character
      // width and using that as a metric works better in some cases but fails in some cases
      // where the spacing worked. So we use both. NOTE: Adobe reader also fails on some of
      // these examples.

      // Keeps track of the previous average character width
      var previousAveCharWidth: Float = -1f;
      while (textIter.hasNext()) {
        val position : TextPosition= textIter.next();
        val current : PositionWrapper = new PositionWrapper(position);
        val characterValue : String= position.getUnicode();

        // Resets the average character width when we see a change in font
        // or a change in the font size
        if (lastPosition != null && (position.getFont() != lastPosition.textPosition.getFont()
          || position.getFontSize() != lastPosition.textPosition.getFontSize())) {
          previousAveCharWidth = -1;
        }

        var positionX: Float = 0f
        var positionY: Float = 0f
        var positionWidth: Float = 0f
        var positionHeight: Float = 0f

        // If we are sorting, then we need to use the text direction
        // adjusted coordinates, because they were used in the sorting.
        if (getSortByPosition())
        {
          positionX = position.getXDirAdj();
          positionY = position.getYDirAdj();
          positionWidth = position.getWidthDirAdj();
          positionHeight = position.getHeightDir();
        }
        else
        {
          positionX = position.getX();
          positionY = position.getY();
          positionWidth = position.getWidth();
          positionHeight = position.getHeight();
        }

        // The current amount of characters in a word
        val wordCharCount : Int = position.getIndividualWidths().length;

        // Estimate the expected width of the space based on the
        // space character with some margin.
        val wordSpacing = position.getWidthOfSpace();
        var deltaSpace: Float = 0;
        if (wordSpacing == 0 || wordSpacing.nan)
        {
          deltaSpace = Float.MaxValue;
        }
        else
        {
          if (lastWordSpacing < 0)
          {
            deltaSpace = wordSpacing * getSpacingTolerance();
          }
          else
          {
            deltaSpace = (wordSpacing + lastWordSpacing) / 2f * getSpacingTolerance();
          }
        }

        // Estimate the expected width of the space based on the average character width
        // with some margin. This calculation does not make a true average (average of
        // averages) but we found that it gave the best results after numerous experiments.
        // Based on experiments we also found that .3 worked well.
        var averageCharWidth: Float = 0;
        if (previousAveCharWidth < 0)
        {
          averageCharWidth = positionWidth / wordCharCount;
        }
        else
        {
          averageCharWidth = (previousAveCharWidth + positionWidth / wordCharCount) / 2f;
        }
        val deltaCharWidth : Float = averageCharWidth * getAverageCharTolerance();

        // Compares the values obtained by the average method and the wordSpacing method
        // and picks the smaller number.
        var expectedStartOfNextWordX : Float= EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE;
        if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE)
        {
          if (deltaCharWidth > deltaSpace)
          {
            expectedStartOfNextWordX = endOfLastTextX + deltaSpace;
          }
          else
          {
            expectedStartOfNextWordX = endOfLastTextX + deltaCharWidth;
          }
        }

        if (lastPosition != null)
        {
          if (startOfArticle) {
            lastPosition.isArticleStart = true
            startOfArticle = false;
          }
          // RDD - Here we determine whether this text object is on the current
          // line. We use the lastBaselineFontSize to handle the superscript
          // case, and the size of the current font to handle the subscript case.
          // Text must overlap with the last rendered baseline text by at least
          // a small amount in order to be considered as being on the same line.

          // XXX BC: In theory, this check should really check if the next char is in
          // full range seen in this line. This is what I tried to do with minYTopForLine,
          // but this caused a lot of regression test failures. So, I'm leaving it be for
          // now
          if (!overlap(positionY, positionHeight, maxYForLine, maxHeightForLine))
          {
            writeLine(normalize(line));
            line.clear();
            lastLineStartPosition = handleLineSeparation(current, lastPosition,
              lastLineStartPosition, maxHeightForLine);
            expectedStartOfNextWordX = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE;
            maxYForLine = MAX_Y_FOR_LINE_RESET_VALUE;
            maxHeightForLine = MAX_HEIGHT_FOR_LINE_RESET_VALUE;
            minYTopForLine = MIN_Y_TOP_FOR_LINE_RESET_VALUE;
          }
          // test if our TextPosition starts after a new word would be expected to start
          if (expectedStartOfNextWordX != EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
            && expectedStartOfNextWordX < positionX &&
            // only bother adding a space if the last character was not a space
            lastPosition.textPosition.getUnicode() != null
            && !lastPosition.textPosition.getUnicode().endsWith(" "))
          {
            line.add(LineItem.getWordSeparator());
          }
        }
        if (positionY >= maxYForLine)
        {
          maxYForLine = positionY;
        }
        // RDD - endX is what PDF considers to be the x coordinate of the
        // end position of the text. We use it in computing our metrics below.
        endOfLastTextX = positionX + positionWidth;

        // add it to the list
        if (characterValue != null)
        {
          if (startOfPage && lastPosition == null)
          {
            writeParagraphStart();// not sure this is correct for RTL?
          }
          line.add(new LineItem(position));
        }
        maxHeightForLine = math.max(maxHeightForLine, positionHeight);
        minYTopForLine = math.min(minYTopForLine, positionY - positionHeight);
        lastPosition = current;
        if (startOfPage)
        {
          lastPosition.setParagraphStart();
          lastPosition.setLineStart();
          lastLineStartPosition = lastPosition;
          startOfPage = false;
        }
        lastWordSpacing = wordSpacing;
        previousAveCharWidth = averageCharWidth;
      }
      // print the final line
      if (line.size() > 0)
      {
        writeLine(normalize(line));
        writeParagraphEnd();
      }
      endArticle();
    }
    writePageEnd();
  }

  def overlap( y1: Float,  height1: Float,  y2: Float,  height2: Float): Boolean = {
    within(y1, y2, .1f) || y2 <= y1 && y2 >= y1 - height1 || y1 <= y2 && y1 >= y2 - height2;
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the lineseparator to the document.
    */
  def  writeLineSeparator() : Unit = {
    output.write(getLineSeparator());
  }

  /**
    * Write the word separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the wordseparator to the document.
    */
  def  writeWordSeparator() : Unit = {
    output.write(getWordSeparator());
  }

  /**
    * Write the string in TextPosition to the output stream.
    *
    * @param text The text to write to the stream.
    * @throws IOException If there is an error when writing the text.
    */
  def  writeCharacters( text: TextPosition) : Unit = {
    output.write(text.getUnicode());
  }

  /**
    * Write a Java string to the output stream. The default implementation will ignore the <code>textPositions</code>
    * and just calls {@link #writeString(String)}.
    *
    * @param text The text to write to the stream.
    * @param textPositions The TextPositions belonging to the text.
    * @throws IOException If there is an error when writing the text.
    */
  def writeString(text: String, textPositions: ju.List[TextPosition]) : Unit = {
    println(s"writeString: ${text}")
    writeString(text);
  }

  /**
    * Write a Java string to the output stream.
    *
    * @param text The text to write to the stream.
    * @throws IOException If there is an error when writing the text.
    */
  def  writeString(text: String) : Unit = {
    output.write(text);
  }

  /**
    * This will determine of two floating point numbers are within a specified variance.
    *
    * @param first The first number to compare to.
    * @param second The second number to compare to.
    * @param variance The allowed variance.
    */
  private def within( first: Float,  second: Float,  variance: Float): Boolean = {
    second < first + variance && second > first - variance;
  }

  /**
    * This will process a TextPosition object and add the text to the list of characters on a page. It takes care of
    * overlapping text.
    *
    * @param text The text to process.
    */
  protected def processTextPosition( text: TextPosition): Unit = {
    var showCharacter : Boolean= true;
    if (suppressDuplicateOverlappingText) {
      showCharacter = false;
      val textCharacter : String= text.getUnicode();
      val textX : Float= text.getX();
      val textY : Float= text.getY();
      var sameTextCharacters: TreeMap[Float, TreeSet[Float]]  = characterListMapping
        .get(textCharacter);
      if (sameTextCharacters == null) {
        sameTextCharacters = new TreeMap[Float, TreeSet[Float]]();
        characterListMapping.put(textCharacter, sameTextCharacters);
      }
      // RDD - Here we compute the value that represents the end of the rendered
      // text. This value is used to determine whether subsequent text rendered
      // on the same line overwrites the current text.
      //
      // We subtract any positive padding to handle cases where extreme amounts
      // of padding are applied, then backed off (not sure why this is done, but there
      // are cases where the padding is on the order of 10x the character width, and
      // the TJ just backs up to compensate after each character). Also, we subtract
      // an amount to allow for kerning (a percentage of the width of the last
      // character).
      var suppressCharacter : Boolean= false;
      val  tolerance : Float = text.getWidth() / textCharacter.length() / 3.0f;

      val xMatches: ju.SortedMap[Float, TreeSet[Float]]  = sameTextCharacters.subMap(textX - tolerance,
        textX + tolerance);
      for (xMatch <- xMatches.values().iterator().asScala if !suppressCharacter) {
        val yMatches: ju.SortedSet[Float]  = xMatch.subSet(textY - tolerance, textY + tolerance);
        if (!yMatches.isEmpty()) {
          suppressCharacter = true;
        }
      }

      if (!suppressCharacter) {
        var ySet: TreeSet[Float]  = sameTextCharacters.get(textX);
        if (ySet == null) {
          ySet = new TreeSet[Float]();
          sameTextCharacters.put(textX, ySet);
        }
        ySet.add(textY);
        showCharacter = true;
      }
    }



    if (showCharacter)
    {
      // if we are showing the character then we need to determine which article it belongs to
      var  foundArticleDivisionIndex : Int= -1;
      var  notFoundButFirstLeftAndAboveArticleDivisionIndex : Int= -1;
      var  notFoundButFirstLeftArticleDivisionIndex : Int= -1;
      var  notFoundButFirstAboveArticleDivisionIndex : Int= -1;
      val  x : Float= text.getX();
      val  y : Float= text.getY();

      if (shouldSeparateByBeads) {
        // for (int i = 0; i < beadRectangles.size() && foundArticleDivisionIndex == -1; i++)
        for (i <- 0 until beadRectangles.size() if foundArticleDivisionIndex == -1) {
          val rect : PDRectangle= beadRectangles.get(i);

          if (rect != null) {
            if (rect.contains(x, y))
            {
              foundArticleDivisionIndex = i * 2 + 1;
            }
            else if ((x < rect.getLowerLeftX() || y < rect.getUpperRightY())
              && notFoundButFirstLeftAndAboveArticleDivisionIndex == -1)
            {
              notFoundButFirstLeftAndAboveArticleDivisionIndex = i * 2;
            }
            else if (x < rect.getLowerLeftX()
              && notFoundButFirstLeftArticleDivisionIndex == -1)
            {
              notFoundButFirstLeftArticleDivisionIndex = i * 2;
            }
            else if (y < rect.getUpperRightY()
              && notFoundButFirstAboveArticleDivisionIndex == -1)
            {
              notFoundButFirstAboveArticleDivisionIndex = i * 2;
            }
          }
          else
          {
            foundArticleDivisionIndex = 0;
          }
        }
      }
      else
      {
        foundArticleDivisionIndex = 0;
      }
      var articleDivisionIndex: Int = 0;
      if (foundArticleDivisionIndex != -1)
      {
        articleDivisionIndex = foundArticleDivisionIndex;
      }
      else if (notFoundButFirstLeftAndAboveArticleDivisionIndex != -1)
      {
        articleDivisionIndex = notFoundButFirstLeftAndAboveArticleDivisionIndex;
      }
      else if (notFoundButFirstLeftArticleDivisionIndex != -1)
      {
        articleDivisionIndex = notFoundButFirstLeftArticleDivisionIndex;
      }
      else if (notFoundButFirstAboveArticleDivisionIndex != -1)
      {
        articleDivisionIndex = notFoundButFirstAboveArticleDivisionIndex;
      }
      else
      {
        articleDivisionIndex = charactersByArticle.size() - 1;
      }

      val textList : ju.List[TextPosition]  = charactersByArticle.get(articleDivisionIndex);

      // In the wild, some PDF encoded documents put diacritics (accents on
      // top of characters) into a separate Tj element. When displaying them
      // graphically, the two chunks get overlayed. With text output though,
      // we need to do the overlay. This code recombines the diacritic with
      // its associated character if the two are consecutive.
      if (textList.isEmpty())
      {
        textList.add(text);
      }
      else
      {
        // test if we overlap the previous entry.
        // Note that we are making an assumption that we need to only look back
        // one TextPosition to find what we are overlapping.
        // This may not always be true. */
        val previousTextPosition : TextPosition= textList.get(textList.size() - 1);
        if (text.isDiacritic() && previousTextPosition.contains(text)) {
          previousTextPosition.mergeDiacritic(text);
        }
        // If the previous TextPosition was the diacritic, merge it into this
        // one and remove it from the list.
        else if (previousTextPosition.isDiacritic() && text.contains(previousTextPosition))
        {
          text.mergeDiacritic(previousTextPosition);
          textList.remove(textList.size() - 1);
          textList.add(text);
        }
        else
        {
          textList.add(text);
        }
      }
    }
  }

  /**
    * This is the page that the text extraction will start on. The pages start at page 1. For example in a 5 page PDF
    * document, if the start page is 1 then all pages will be extracted. If the start page is 4 then pages 4 and 5 will
    * be extracted. The default value is 1.
    *
    * @return Value of property startPage.
    */
  def getStartPage(): Int = {
    return startPageNum;
  }

  /**
    * This will set the first page to be extracted by this class.
    *
    * @param startPageValue New value of 1-based startPage property.
    */
  def  setStartPage( startPageValue: Int): Unit = {
    startPageNum = startPageValue;
  }

  /**
    * This will get the last page that will be extracted. This is inclusive, for example if a 5 page PDF an endPage
    * value of 5 would extract the entire document, an end page of 2 would extract pages 1 and 2. This defaults to
    * Integer.MAX_VALUE such that all pages of the pdf will be extracted.
    *
    * @return Value of property endPage.
    */
  def  getEndPage(): Int = {
    endPage;
  }

  /**
    * This will set the last page to be extracted by this class.
    *
    * @param endPageValue New value of 1-based endPage property.
    */
  def setEndPage( endPageValue: Int): Unit = {
    endPage = endPageValue;
  }

  /**
    * Set the desired line separator for output text. The line.separator system property is used if the line separator
    * preference is not set explicitly using this method.
    *
    * @param separator The desired line separator string.
    */
  def  setLineSeparator( separator: String): Unit = {
    lineSeparator = separator;
  }

  /**
    * This will get the line separator.
    *
    * @return The desired line separator string.
    */
  def getLineSeparator(): String = {
    return lineSeparator;
  }

  /**
    * This will get the word separator.
    *
    * @return The desired word separator string.
    */
  def getWordSeparator(): String = {
    wordSeparator;
  }

  /**
    * Set the desired word separator for output text. The PDFBox text extraction algorithm will output a space
    * character if there is enough space between two words. By default a space character is used. If you need and
    * accurate count of characters that are found in a PDF document then you might want to set the word separator to
    * the empty string.
    *
    * @param separator The desired page separator string.
    */
  def  setWordSeparator( separator: String): Unit = {
    wordSeparator = separator;
  }

  /**
    * @return Returns the suppressDuplicateOverlappingText.
    */
  def getSuppressDuplicateOverlappingText(): Boolean = {
    return suppressDuplicateOverlappingText;
  }

  /**
    * Get the current page number that is being processed.
    *
    * @return A 1 based number representing the current page.
    */
  def getCurrentPageNo(): Int = {
    return currentPageNo;
  }

  /**
    * The output stream that is being written to.
    *
    * @return The stream that output is being written to.
    */
  def  getOutput(): Writer = {
    return output;
  }

  /**
    * Character strings are grouped by articles. It is quite common that there will only be a single article. This
    * returns a List that contains List objects, the inner lists will contain TextPosition objects.
    *
    * @return A double List of TextPositions for all text strings on the page.
    */
  def  getCharactersByArticle(): ju.List[ju.List[TextPosition]] = {
    return charactersByArticle;
  }

  /**
    * By default the text stripper will attempt to remove text that overlapps each other. Word paints the same
    * character several times in order to make it look bold. By setting this to false all text will be extracted, which
    * means that certain sections will be duplicated, but better performance will be noticed.
    *
    * @param suppressDuplicateOverlappingTextValue The suppressDuplicateOverlappingText to set.
    */
  def  setSuppressDuplicateOverlappingText( suppressDuplicateOverlappingTextValue: Boolean): Unit = {
    suppressDuplicateOverlappingText = suppressDuplicateOverlappingTextValue;
  }

  /**
    * This will tell if the text stripper should separate by beads.
    *
    * @return If the text will be grouped by beads.
    */
  def  getSeparateByBeads(): Boolean = {
    return shouldSeparateByBeads;
  }

  /**
    * Set if the text stripper should group the text output by a list of beads. The default value is true!
    *
    * @param aShouldSeparateByBeads The new grouping of beads.
    */
  def  setShouldSeparateByBeads( aShouldSeparateByBeads: Boolean): Unit = {
    shouldSeparateByBeads = aShouldSeparateByBeads;
  }

  /**
    * Get the bookmark where text extraction should end, inclusive. Default is null.
    *
    * @return The ending bookmark.
    */
  def getEndBookmark(): PDOutlineItem = {
    return endBookmark;
  }

  /**
    * Set the bookmark where the text extraction should stop.
    *
    * @param aEndBookmark The ending bookmark.
    */
  def  setEndBookmark( aEndBookmark: PDOutlineItem): Unit = {
    endBookmark = aEndBookmark;
  }

  /**
    * Get the bookmark where text extraction should start, inclusive. Default is null.
    *
    * @return The starting bookmark.
    */
  def  getStartBookmark(): PDOutlineItem = {
    return startBookmark;
  }

  /**
    * Set the bookmark where text extraction should start, inclusive.
    *
    * @param aStartBookmark The starting bookmark.
    */
  def  setStartBookmark( aStartBookmark: PDOutlineItem): Unit = {
    startBookmark = aStartBookmark;
  }

  /**
    * This will tell if the text stripper should add some more text formatting.
    *
    * @return true if some more text formatting will be added
    */
  def getAddMoreFormatting(): Boolean = {
    return addMoreFormatting;
  }

  /**
    * There will some additional text formatting be added if addMoreFormatting is set to true. Default is false.
    *
    * @param newAddMoreFormatting Tell PDFBox to add some more text formatting
    */
  def  setAddMoreFormatting( newAddMoreFormatting: Boolean): Unit = {
    addMoreFormatting = newAddMoreFormatting;
  }

  /**
    * This will tell if the text stripper should sort the text tokens before writing to the stream.
    *
    * @return true If the text tokens will be sorted before being written.
    */
  def getSortByPosition(): Boolean = {
    return sortByPosition;
  }

  /**
    * The order of the text tokens in a PDF file may not be in the same as they appear visually on the screen. For
    * example, a PDF writer may write out all text by font, so all bold or larger text, then make a second pass and
    * write out the normal text.<br>
    * The default is to <b>not</b> sort by position.<br>
    * <br>
    * A PDF writer could choose to write each character in a different order. By default PDFBox does <b>not</b> sort
    * the text tokens before processing them due to performance reasons.
    *
    * @param newSortByPosition Tell PDFBox to sort the text positions.
    */
  def  setSortByPosition( newSortByPosition: Boolean): Unit = {
    sortByPosition = newSortByPosition;
  }

  /**
    * Get the current space width-based tolerance value that is being used to estimate where spaces in text should be
    * added. Note that the default value for this has been determined from trial and error.
    *
    * @return The current tolerance / scaling factor
    */
  def  getSpacingTolerance(): Float = {
    return spacingTolerance;
  }

  /**
    * Set the space width-based tolerance value that is used to estimate where spaces in text should be added. Note
    * that the default value for this has been determined from trial and error. Setting this value larger will reduce
    * the number of spaces added.
    *
    * @param spacingToleranceValue tolerance / scaling factor to use
    */
  def  setSpacingTolerance( spacingToleranceValue: Float): Unit = {
    spacingTolerance = spacingToleranceValue;
  }

  /**
    * Get the current character width-based tolerance value that is being used to estimate where spaces in text should
    * be added. Note that the default value for this has been determined from trial and error.
    *
    * @return The current tolerance / scaling factor
    */
  def  getAverageCharTolerance(): Float = {
    return averageCharTolerance;
  }

  /**
    * Set the character width-based tolerance value that is used to estimate where spaces in text should be added. Note
    * that the default value for this has been determined from trial and error. Setting this value larger will reduce
    * the number of spaces added.
    *
    * @param averageCharToleranceValue average tolerance / scaling factor to use
    */
  def  setAverageCharTolerance( averageCharToleranceValue: Float):Unit = {
    averageCharTolerance = averageCharToleranceValue;
  }

  /**
    * returns the multiple of whitespace character widths for the current text which the current line start can be
    * indented from the previous line start beyond which the current line start is considered to be a paragraph start.
    *
    * @return the number of whitespace character widths to use when detecting paragraph indents.
    */
  def  getIndentThreshold(): Float = {
    return indentThreshold;
  }

  /**
    * sets the multiple of whitespace character widths for the current text which the current line start can be
    * indented from the previous line start beyond which the current line start is considered to be a paragraph start.
    * The default value is 2.0.
    *
    * @param indentThresholdValue the number of whitespace character widths to use when detecting paragraph indents.
    */
  def  setIndentThreshold( indentThresholdValue: Float): Unit = {
    indentThreshold = indentThresholdValue;
  }

  /**
    * the minimum whitespace, as a multiple of the max height of the current characters beyond which the current line
    * start is considered to be a paragraph start.
    *
    * @return the character height multiple for max allowed whitespace between lines in the same paragraph.
    */
  def  getDropThreshold(): Float = {
    return dropThreshold;
  }

  /**
    * sets the minimum whitespace, as a multiple of the max height of the current characters beyond which the current
    * line start is considered to be a paragraph start. The default value is 2.5.
    *
    * @param dropThresholdValue the character height multiple for max allowed whitespace between lines in the same
    * paragraph.
    */
  def  setDropThreshold( dropThresholdValue: Float): Unit = {
    dropThreshold = dropThresholdValue;
  }

  /**
    * Returns the string which will be used at the beginning of a paragraph.
    *
    * @return the paragraph start string
    */
  def  getParagraphStart(): String = {
    return paragraphStart;
  }

  /**
    * Sets the string which will be used at the beginning of a paragraph.
    *
    * @param s the paragraph start string
    */
  def  setParagraphStart( s: String): Unit = {
    paragraphStart = s;
  }

  /**
    * Returns the string which will be used at the end of a paragraph.
    *
    * @return the paragraph end string
    */
  def  getParagraphEnd(): String = {
    return paragraphEnd;
  }

  /**
    * Sets the string which will be used at the end of a paragraph.
    *
    * @param s the paragraph end string
    */
  def  setParagraphEnd( s: String): Unit = {
    paragraphEnd = s;
  }

  /**
    * Returns the string which will be used at the beginning of a page.
    *
    * @return the page start string
    */
  def  getPageStart(): String = {
    return pageStart;
  }

  /**
    * Sets the string which will be used at the beginning of a page.
    *
    * @param pageStartValue the page start string
    */
  def  setPageStart( pageStartValue: String): Unit = {
    pageStart = pageStartValue;
  }

  /**
    * Returns the string which will be used at the end of a page.
    *
    * @return the page end string
    */
  def  getPageEnd(): String = {
    return pageEnd;
  }

  /**
    * Sets the string which will be used at the end of a page.
    *
    * @param pageEndValue the page end string
    */
  def  setPageEnd( pageEndValue: String): Unit = {
    pageEnd = pageEndValue;
  }

  // /**
  //  * Returns the string which will be used at the beginning of an article.
  //  *
  //  * @return the article start string
  //  */
  // def String getArticleStart()
  // {
  //     return articleStart;
  // }

  // /**
  //  * Sets the string which will be used at the beginning of an article.
  //  *
  //  * @param articleStartValue the article start string
  //  */
  // def  setArticleStart(String articleStartValue)
  // {
  //     articleStart = articleStartValue;
  // }

  // /**
  //  * Returns the string which will be used at the end of an article.
  //  *
  //  * @return the article end string
  //  */
  // public String getArticleEnd()
  // {
  //     return articleEnd;
  // }

  // /**
  //  * Sets the string which will be used at the end of an article.
  //  *
  //  * @param articleEndValue the article end string
  //  */
  // def  setArticleEnd(String articleEndValue)
  // {
  //     articleEnd = articleEndValue;
  // }

  /**
    * handles the line separator for a new line given the specified current and previous TextPositions.
    *
    * @param current the current text position
    * @param lastPosition the previous text position
    * @param lastLineStartPosition the last text position that followed a line separator.
    * @param maxHeightForLine max height for positions since lastLineStartPosition
    * @return start position of the last line
    * @throws IOException if something went wrong
    */
  private def handleLineSeparation(
    current: PositionWrapper,
    lastPosition: PositionWrapper,
    _lastLineStartPosition: PositionWrapper,
    maxHeightForLine: Float
  ) : PositionWrapper = {

    var lastLineStartPosition: PositionWrapper = _lastLineStartPosition
    current.setLineStart();
    isParagraphSeparation(current, lastPosition, lastLineStartPosition, maxHeightForLine);
    lastLineStartPosition = current;
    if (current.isParagraphStart) {
      if (lastPosition.isArticleStart) {
        if (lastPosition.isLineStart) {
          writeLineSeparator();
        }
        writeParagraphStart();
      } else {
        writeLineSeparator();
        writeParagraphSeparator();
      }
    } else {
      writeLineSeparator();
    }
    return lastLineStartPosition;
  }

  /**
    * tests the relationship between the last text position, the current text position and the last text position that
    * followed a line separator to decide if the gap represents a paragraph separation. This should <i>only</i> be
    * called for consecutive text positions that first pass the line separation test.
    * <p>
    * This base implementation tests to see if the lastLineStartPosition is null OR if the current vertical position
    * has dropped below the last text vertical position by at least 2.5 times the current text height OR if the current
    * horizontal position is indented by at least 2 times the current width of a space character.
    * </p>
    * <p>
    * This also attempts to identify text that is indented under a hanging indent.
    * </p>
    * <p>
    * This method sets the isParagraphStart and isHangingIndent flags on the current position object.
    * </p>
    *
    * @param position the current text position. This may have its isParagraphStart or isHangingIndent flags set upon
    * return.
    * @param lastPosition the previous text position (should not be null).
    * @param lastLineStartPosition the last text position that followed a line separator, or null.
    * @param maxHeightForLine max height for text positions since lasLineStartPosition.
    */
  def  isParagraphSeparation( position: PositionWrapper,  lastPosition: PositionWrapper,
    lastLineStartPosition: PositionWrapper,  maxHeightForLine: Float): Unit = {
    var result : Boolean= false;
    if (lastLineStartPosition == null) {
      result = true;
    } else {
      val yGap : Float= Math.abs(position.textPosition.getYDirAdj()
        - lastPosition.textPosition.getYDirAdj());
      val newYVal : Float= multiplyFloat(getDropThreshold(), maxHeightForLine);
      // do we need to flip this for rtl?
      val xGap : Float= position.textPosition.getXDirAdj()
        - lastLineStartPosition.textPosition.getXDirAdj();
      val newXVal : Float= multiplyFloat(getIndentThreshold(),
        position.textPosition.getWidthOfSpace());
      val positionWidth : Float= multiplyFloat(0.25f, position.textPosition.getWidth());

      if (yGap > newYVal) {
        result = true;
      } else if (xGap > newXVal) {
        // text is indented, but try to screen for hanging indent
        if (!lastLineStartPosition.isParagraphStart) {
          result = true;
        } else {
          position.setHangingIndent();
        }
      } else if (xGap < -position.textPosition.getWidthOfSpace()) {
        // text is left of previous line. Was it a hanging indent?
        if (!lastLineStartPosition.isParagraphStart) {
          result = true;
        }
      } else if (Math.abs(xGap) < positionWidth) {
        // current horizontal position is within 1/4 a char of the last
        // linestart. We'll treat them as lined up.
        if (lastLineStartPosition.isHangingIndent) {
          position.setHangingIndent();
        } else if (lastLineStartPosition.isParagraphStart) {
          // check to see if the previous line looks like
          // any of a number of standard list item formats
          val liPattern : Pattern= matchListItemPattern(lastLineStartPosition);
          if (liPattern != null) {
            val currentPattern : Pattern= matchListItemPattern(position);
            if (liPattern == currentPattern) {
              result = true;
            }
          }
        }
      }
    }
    if (result) {
      position.setParagraphStart();
    }
  }

  def  multiplyFloat( value1: Float,  value2: Float): Float = {
    // multiply 2 floats and truncate the resulting value to 3 decimal places
    // to avoid wrong results when comparing with another float
    return Math.round(value1 * value2 * 1000) / 1000f;
  }

  /**
    * writes the paragraph separator string to the output.
    *
    * @throws IOException if something went wrong
    */
  def  writeParagraphSeparator() : Unit =
  {
    writeParagraphEnd();
    writeParagraphStart();
  }

  /**
    * Write something (if defined) at the start of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  def  writeParagraphStart() : Unit =
  {
    if (inParagraph)
    {
      writeParagraphEnd();
      inParagraph = false;
    }
    output.write(getParagraphStart());
    inParagraph = true;
  }

  /**
    * Write something (if defined) at the end of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  def  writeParagraphEnd() : Unit =
  {
    if (!inParagraph)
    {
      writeParagraphStart();
    }
    output.write(getParagraphEnd());
    inParagraph = false;
  }

  /**
    * Write something (if defined) at the start of a page.
    *
    * @throws IOException if something went wrong
    */
  def  writePageStart() : Unit =
  {
    output.write(getPageStart());
  }

  /**
    * Write something (if defined) at the end of a page.
    *
    * @throws IOException if something went wrong
    */
  def  writePageEnd() : Unit =
  {
    output.write(getPageEnd());
  }

  /**
    * returns the list item Pattern object that matches the text at the specified PositionWrapper or null if the text
    * does not match such a pattern. The list of Patterns tested against is given by the {@link #getListItemPatterns()}
    * method. To add to the list, simply override that method (if sub-classing) or explicitly supply your own list
    * using {@link #setListItemPatterns(List)}.
    *
    * @param pw position
    * @return the matching pattern
    */
  def  matchListItemPattern( pw: PositionWrapper): Pattern = {
    val tp : TextPosition = pw.textPosition;
    val txt : String= tp.getUnicode();
    return matchPattern(txt, getListItemPatterns());
  }

  /**
    * a list of regular expressions that match commonly used list item formats, i.e. bullets, numbers, letters, Roman
    * numerals, etc. Not meant to be comprehensive.
    */
  val LIST_ITEM_EXPRESSIONS: List[String]  = List(
    "\\.", "\\d+\\.", "\\[\\d+\\]",
    "\\d+\\)", "[A-Z]\\.", "[a-z]\\.", "[A-Z]\\)", "[a-z]\\)", "[IVXL]+\\.",
    "[ivxl]+\\."
  )

  var listOfPatterns: ju.List[Pattern]  = null;

  /**
    * use to supply a different set of regular expression patterns for matching list item starts.
    *
    * @param patterns list of patterns
    */
  def  setListItemPatterns(patterns: ju.List[Pattern] ): Unit = {
    listOfPatterns = patterns;
  }

  /**
    * returns a list of regular expression Patterns representing different common list item formats. For example
    * numbered items of form:
    * <ol>
    * <li>some text</li>
    * <li>more text</li>
    * </ol>
    * or
    * <ul>
    * <li>some text</li>
    * <li>more text</li>
    * </ul>
    * etc., all begin with some character pattern. The pattern "\\d+\." (matches "1.", "2.", ...) or "\[\\d+\]"
    * (matches "[1]", "[2]", ...).
    * <p>
    * This method returns a list of such regular expression Patterns.
    *
    * @return a list of Pattern objects.
    */
  def getListItemPatterns(): ju.List[Pattern] = {
    if (listOfPatterns == null) {
      listOfPatterns = new ArrayList[Pattern]();
      for ( expression <- LIST_ITEM_EXPRESSIONS) {
        val p : Pattern= Pattern.compile(expression);
        listOfPatterns.add(p);
      }
    }
    return listOfPatterns;
  }

  /**
    * iterates over the specified list of Patterns until it finds one that matches the specified string. Then returns
    * the Pattern.
    * <p>
    * Order of the supplied list of patterns is important as most common patterns should come first. Patterns should be
    * strict in general, and all will be used with case sensitivity on.
    * </p>
    *
    * @param string the string to be searched
    * @param patterns list of patterns
    * @return matching pattern
    */
  def  matchPattern( string: String, patterns: ju.List[Pattern]) : Pattern = {
    for (p <- patterns.iterator().asScala) {
      if (p.matcher(string).matches()) {
        return p;
      }
    }
    return null;
  }

  /**
    * Write a list of string containing a whole line of a document.
    *
    * @param line a list with the words of the given line
    * @throws IOException if something went wrong
    */
  def  writeLine(line : ju.List[WordWithTextPositions]): Unit = {
    val numberOfStrings : Int = line.size();

    for (i <-  0 until numberOfStrings) {
      val word : WordWithTextPositions  = line.get(i);
      writeString(word.getText(), word.getTextPositions());
      if (i < numberOfStrings - 1) {
        writeWordSeparator();
      }
    }
  }

  /**
    * Normalize the given list of TextPositions.
    *
    * @param line list of TextPositions
    * @return a list of strings, one string for every word
    */
  def normalize(line: ju.List[LineItem]): ju.List[WordWithTextPositions] = {
    val normalized: ju.List[WordWithTextPositions]  = new ju.LinkedList[WordWithTextPositions]();
    var lineBuilder : StringBuilder= new StringBuilder();
    val wordPositions: ju.List[TextPosition]  = new ArrayList[TextPosition]();

    for ( item <- line.iterator().asScala) {
      lineBuilder = normalizeAdd(normalized, lineBuilder, wordPositions, item);
    }

    if (lineBuilder.length() > 0) {
      normalized.add(createWord(lineBuilder.toString(), wordPositions));
    }
    return normalized;
  }

  /**
    * Handles the LTR and RTL direction of the given words. The whole implementation stands and falls with the given
    * word. If the word is a full line, the results will be the best. If the word contains of single words or
    * characters, the order of the characters in a word or words in a line may wrong, due to RTL and LTR marks and
    * characters!
    *
    * Based on http://www.nesterovsky-bros.com/weblog/2013/07/28/VisualToLogicalConversionInJava.aspx
    *
    * @param word The word that shall be processed
    * @return new word with the correct direction of the containing characters
    */
  def  handleDirection( word: String): String = {
    val bidi : Bidi= new Bidi(word, Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT);

    // if there is pure LTR text no need to process further
    if (!bidi.isMixed() && bidi.getBaseLevel() == Bidi.DIRECTION_LEFT_TO_RIGHT) {
      return word;
    }

    // collect individual bidi information
    val runCount : Int= bidi.getRunCount();
    // Array.fill
    val levels: Array[Byte] =  Array.ofDim[Byte](runCount)
    val runs: Array[Integer]  =  Array.ofDim[Integer](runCount)

    for ( i <- 0 until runCount) {
      levels(i) = bidi.getRunLevel(i).toByte
      runs(i) = i;
    }


    // reorder individual parts based on their levels
    Bidi.reorderVisually(levels, 0, runs.asInstanceOf[Array[Object]], 0, runCount);

    // collect the parts based on the direction within the run
    val result : StringBuilder= new StringBuilder();

    for ( i <- 0 until runCount) {
      val index : Int= runs(i)
      val start : Int= bidi.getRunStart(index);
      var end : Int= bidi.getRunLimit(index);

      val level : Int = levels(index).toInt

      if ((level & 1) != 0) {
        while (end-1 >= start) {
          end = end-1
          val character = word.charAt(end);
          if (Character.isMirrored(word.codePointAt(end))) {
            if (MIRRORING_CHAR_MAP.containsKey(character)) {
              result.append(MIRRORING_CHAR_MAP.get(character));
            } else {
              result.append(character);
            }
          } else {
            result.append(character);
          }
        }
      } else {
        result.append(word, start, end);
      }
    }

    return result.toString();
  }

  val MIRRORING_CHAR_MAP = new ju.HashMap[Character, Character]();

  {
    val path = "org/apache/pdfbox/resources/text/BidiMirroring.txt";
    val cls = classOf[PDFTextStripper]
    val input : InputStream = cls.getClassLoader().getResourceAsStream(path);
    try {
      parseBidiFile(input);
    } catch {

      case  e: IOException =>
        println("Could not parse BidiMirroring.txt, mirroring char map will be empty: " + e.getMessage());
    } finally {
      try {
        input.close();
      } catch {
        case  e: IOException =>
          println("Could not close BidiMirroring.txt " + e);
      }
    }
  }

  /**
    * This method parses the bidi file provided as inputstream.
    *
    * @param inputStream - The bidi file as inputstream
    * @throws IOException if any line could not be read by the LineNumberReader
    */
  def parseBidiFile(inputStream: InputStream) : Unit = {
    val source = io.Source.fromInputStream(inputStream)
    for {
      l <- source.getLines
    } {
      val (chars, comment) = l.span(_ != '#')
      val line = chars.mkString

      if (line.nonEmpty) {
        val fields = line.split(";").map{ t =>
          Integer.parseInt(t.trim(), 16).toChar
        }

        if (fields.length == 2) {
          // initialize the MIRRORING_CHAR_MAP
          MIRRORING_CHAR_MAP.put(fields(0), fields(1));
        }

      }

    }

    // val rd : LineNumberReader= new LineNumberReader(new InputStreamReader(inputStream));
    // do {
    //   var s : String= rd.readLine();
    //   if (s == null) {
    //     break;
    //   }

    //   val comment : Int= s.indexOf('#'); // ignore comments

    //   if (comment != -1) {
    //     s = s.substring(0, comment);
    //   }

    //   if (s.length() < 2) {
    //     continue;
    //   }

    //   val st : StringTokenizer= new StringTokenizer(s, ";");
    //   val nFields : Int= st.countTokens();
    //   val fields: ju.Array[Character] = Array[Character](nFields)

    //   for (i <- 0 until nFields) {
    //     fields(i) =  Integer.parseInt(st.nextToken().trim(), 16);
    //   }

    //   if (fields.length == 2) {
    //     // initialize the MIRRORING_CHAR_MAP
    //     MIRRORING_CHAR_MAP.put(fields(0), fields(1));
    //   }

    // } while (true);

  }

  /**
    * Used within {@link #normalize(List)} to create a single {@link WordWithTextPositions} entry.
    */
  def createWord(word: String, wordPositions: ju.List[TextPosition]): WordWithTextPositions = {
    return new WordWithTextPositions(normalizeWord(word), wordPositions);
  }

  /**
    * Normalize certain Unicode characters. For example, convert the single "fi" ligature to "f" and "i". Also
    * normalises Arabic and Hebrew presentation forms.
    *
    * @param word Word to normalize
    * @return Normalized word
    */
  def  normalizeWord( word: String): String = {
    var builder : StringBuilder= null;
    var p : Int= 0;
    val strLength : Int= word.length();
    val q = strLength

    for ( q <- 0 until strLength) {
      // We only normalize if the codepoint is in a given range.
      // Otherwise, NFKC converts too many things that would cause
      // confusion. For example, it converts the micro symbol in
      // extended Latin to the value in the Greek script. We normalize
      // the Unicode Alphabetic and Arabic A&B Presentation forms.
      val c : Char= word.charAt(q);
      if (0xFB00 <= c && c <= 0xFDFF || 0xFE70 <= c && c <= 0xFEFF) {
        if (builder == null)
        {
          builder = new StringBuilder(strLength * 2);
        }
        builder.append(word.substring(p, q));
        // Some fonts map U+FDF2 differently than the Unicode spec.
        // They add an extra U+0627 character to compensate.
        // This removes the extra character for those fonts.
        if (c == 0xFDF2 && q > 0
          && (word.charAt(q - 1) == 0x0627 || word.charAt(q - 1) == 0xFE8D))
        {
          builder.append("\u0644\u0644\u0647");
        }
        else
        {
          // Trim because some decompositions have an extra space, such as U+FC5E
          builder.append(Normalizer
            .normalize(word.substring(q, q + 1), Normalizer.Form.NFKC).trim());
        }
        p = q + 1;
      }
    }

    if (builder == null) {
      return handleDirection(word);
    } else {
      builder.append(word.substring(p, q));
      return handleDirection(builder.toString());
    }
  }

  /**
    * Used within {@link #normalize(List)} to handle a {@link TextPosition}.
    *
    * @return The StringBuilder that must be used when calling this method.
    */
  def normalizeAdd(
    normalized: ju.List[WordWithTextPositions],
    _lineBuilder: StringBuilder,
    wordPositions: ju.List[TextPosition] ,
    item: LineItem
  ): StringBuilder = {
    var lineBuilder: StringBuilder = _lineBuilder
    if (item.isWordSeparator()) {
      normalized.add(
        createWord(lineBuilder.toString(), new ArrayList[TextPosition](wordPositions)));
      lineBuilder = new StringBuilder();
      wordPositions.clear();
    } else {
      val text : TextPosition= item.textPosition;
      lineBuilder.append(text.getUnicode());
      wordPositions.add(text);
    }

    lineBuilder;
  }


  // protected void showFontGlyph(Matrix textRenderingMatrix, PDFont font, int code, String unicode,
  //   Vector displacement) throws IOException
  // {
  override protected def showFontGlyph(
    textRenderingMatrix: Matrix,
    font: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {
    println("showFontGlyph");
  }

  override protected def showGlyph(
    textRenderingMatrix: Matrix,
    font: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {
    println("showGlyph");
    super.showGlyph(textRenderingMatrix, font, code, unicode, displacement);
    println(unicode);
  }

}

/**
  * internal marker class. Used as a place holder in a line of TextPositions.
  */
object LineItem {

  val WORD_SEPARATOR = new LineItem();

  def  getWordSeparator(): LineItem = {
    WORD_SEPARATOR;
  }
}
class LineItem(
  var  textPosition: TextPosition = null
) {


  def  getTextPosition(): TextPosition = {
    return textPosition;
  }

  def  isWordSeparator(): Boolean  = {
    return textPosition == null;
  }
}

/**
  * Internal class that maps strings to lists of {@link TextPosition} arrays. Note that the number of entries in that
  * list may differ from the number of characters in the string due to normalization.
  *
  * @author Axel Dörfler
  */
class WordWithTextPositions(
  word: String,
  positions: ju.List[TextPosition]
) {
  var text: String = word;
  var textPositions: ju.List[TextPosition] = positions

  def  getText(): String = {
    return text;
  }

  def getTextPositions(): ju.List[TextPosition] = {
    return textPositions;
  }
}

/**
  * wrapper of TextPosition that adds flags to track status as linestart and paragraph start positions.
  * <p>
  * This is implemented as a wrapper since the TextPosition class doesn't provide complete access to its state fields
  * to subclasses. Also, conceptually TextPosition is immutable while these flags need to be set post-creation so it
  * makes sense to put these flags in this separate class.
  * </p>
  *
  * @author m.martinez@ll.mit.edu
  */

class PositionWrapper(
  val textPosition: TextPosition
) {
  var  isLineStart : Boolean= false;
  var  isParagraphStart : Boolean= false;
  var  isPageBreak : Boolean= false;
  var  isHangingIndent : Boolean= false;
  var  isArticleStart : Boolean= false;



  /**
    * Returns the underlying TextPosition object.
    *
    * @return the text position
    */
  def  getTextPosition(): TextPosition = {
    return textPosition;
  }

  // def  isLineStart(): Boolean = {
  //   return isLineStart;
  // }

  /**
    * Sets the isLineStart() flag to true.
    */
  def  setLineStart(): Unit = {
    this.isLineStart = true;
  }

  // def  isParagraphStart(): Boolean = {
  //   return isParagraphStart;
  // }

  /**
    * sets the isParagraphStart() flag to true.
    */
  def setParagraphStart(): Unit = {
    this.isParagraphStart = true;
  }

  // def  isArticleStart(): Boolean
  // {
  //   return isArticleStart;
  // }

  /**
    * Sets the isArticleStart() flag to true.
    */
  def  setArticleStart(): Unit = {
    this.isArticleStart = true;
  }

  // def  isPageBreak()
  // {
  //   return isPageBreak;
  // }

  /**
    * Sets the isPageBreak() flag to true.
    */
  def  setPageBreak(): Unit = {
    this.isPageBreak = true;
  }

  // def Boolean isHangingIndent()
  // {
  //   return isHangingIndent;
  // }

  /**
    * Sets the isHangingIndent() flag to true.
    */
  def  setHangingIndent(): Unit = {
    this.isHangingIndent = true;
  }
}

