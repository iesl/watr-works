package org.watrworks
package segment

import org.scalatest.diagrams.Diagrams
import java.util.stream

class LineTokenizationTest extends SegmentationTestUtils with Diagrams {
  // N.B. this paper removed from test cases b/c the visible text is actually image overlays, w/ some hand-entered text
  //   """Page:0 file:///Schauer-1987.pdf""",
  behavior of "text line identification"
  val testExamples = List(
    TextExample(
      """Page:0 /saccharomyces-1-page.pdf""",
      """Daniel A. Skelly^{1,3}, Paul M. Magwene1 , Brianna Meeks^{2} , Helen A. Murphy^{2} (0.0, 160.0, 900.0, 50.0)""",
      """Daniel A. Skelly^{1,3}, Paul M. Magwene1 , Brianna Meeks^{2} , Helen A. Murphy^{2}"""
    ),
    TextExample(
      """Page:7 /101016jactamat201501032.pdf""",
      """and 10 ^{5} s ^{1}. In general, the three curves exhibit a plateau  (305.52, 172.83, 245.0, 10.0)""",
      """and 10^{5} s^{1}. In general, the three curves exhibit a plateau"""
    ),
    TextExample(
      """Page:0 /101016jcarbon201301056.pdf""",
      """One-pot synthesis of uniform Fe3O4 nanocrystals (47.28, 159.86, 397.48, 17.71) """,
      """One-pot synthesis of uniform Fe_{3}O_{4} nanocrystals"""
    ),
    TextExample(
      """Page:1 /acsnano.5b00028.pdf""",
      """and epithelial absorption require very di^{ff}erent surface   (l:55.74, t:222.64, w:201.23, h:10.84)""",
      """and epithelial absorption require very different surface"""
    ),
    TextExample(
      """Page:0 file:///101016japsusc201210126.pdf""",
      """http://dx.doi.org/10.1016/j.apsusc.2012.10.126 (l:32.73, t:737.54, w:142.92, h:6.36)""",
      """http://dx.doi.org/10.1016/j.apsusc.2012.10.126"""
    ),
    TextExample(
      """Page:1 file:///101016japsusc201210126.pdf""",
      """ P t^{0}by sodium formate. The Pt/PANi nanocomposite was assessed    (l:41.92, t:305.87, w:251.55, h:9.28) """,
      """Pt^{0} by sodium formate. The Pt/PANi nanocomposite was assessed"""
    ),
    TextExample(
      """Page:0 file:///101016japsusc201210126.pdf""",
      """ C om p o s i te  (l:32.73, t:376.02, w:32.54, h:6.36) """,
      """Composite"""
    ),
    TextExample(
      """|Page:0 /101016japsusc201210126.pdf
         |""".stripMargin,
      """|   (l:32.23, t:488.00, w:251.54, h:18.43)
         |""".stripMargin,
      """|Proton exchange membrane fuel cell (PEMFC) is considered to
         |be the most attractive energy technology for the future due to
         |""".stripMargin
    ),
    TextExample(
      """|Page:0 /101016jactamat201112024.pdf
         |""".stripMargin,
      """|the                                                         (l:520.27, t:465.98, w:13.17, h:9.96)
         |Perhaps                                                     (l:477.92, t:465.98, w:34.10, h:9.96)
         |variables.                                                  (l:429.05, t:465.98, w:40.61, h:9.96)
         |and                                                         (l:374.11, t:465.98, w:15.98, h:9.96)
         |ing),                                                       (l:311.53, t:465.98, w:19.18, h:9.96)
         |other                                                       (l:398.32, t:465.98, w:22.51, h:9.96)
         |strain,                                                     (l:338.97, t:465.98, w:26.85, h:9.96)
         |most                                                        (l:541.64, t:465.98, w:20.84, h:9.96)
         |""".stripMargin,
      """|ing), strain, and other variables. Perhaps the most
         |""".stripMargin
    ),
    TextExample(
      """|Page:0 /101016jactamat200401025.pdf
         |""".stripMargin,
      """|A bs t r a c t               (l:42.52, t:294.50, w:32.20, h:8.97)
         |""".stripMargin,
      """|Abstract
         |""".stripMargin
    ),
    TextExample(
      """|Page:0 /101016jactamat200401025.pdf
         |""".stripMargin,
      """|safet y       (l:519.76, t:452.43, w:24.88, h:9.96)
         |c om b i ne   (l:334.94, t:452.43, w:36.08, h:9.96)
         |and           (l:498.16, t:452.43, w:15.98, h:9.96)
         |at            (l:376.78, t:452.43, w:8.30, h:9.96)
         |To            (l:317.14, t:452.43, w:12.13, h:9.96)
         |b es t        (l:390.73, t:452.43, w:17.01, h:9.96)
         |weld ing      (l:459.55, t:452.43, w:32.96, h:9.96)
         |e co n o mi c (l:413.40, t:452.43, w:40.44, h:9.96)
         |""".stripMargin,
      """|To combine at best economic welding and safety
         |""".stripMargin
    ),
    TextExample(
      """| Page:1 /101016jactamat201501032.pdf
         |""".stripMargin,
      """|C.M. Cepeda-Jiménez et al. / Acta Materialia 88 (2015) 232–244               (l:183.34, t:47.54, w:220.59, h:8.08)
         |""".stripMargin,
      """|C.M. Cepeda-Jiménez et al. / Acta Materialia 88 (2015) 232–244
         |""".stripMargin
    ),
    TextExample(
      """|Page:1 /101016jactamat201501032.pdf
         |""".stripMargin,
      """| grain size of 1 lm in               (l:42.52, t:76.04, w:87.32, h:9.84)
         | pure               (l:133.74, t:76.04, w:18.23, h:9.46)
         | Mg samples fabricated               (l:155.91, t:76.04, w:94.32, h:9.46)
         | by               (l:254.15, t:76.04, w:9.96, h:9.46)
         | hot               (l:268.04, t:76.04, w:13.60, h:9.46)
         |""".stripMargin,
      """|grain size of 1 lm in pure Mg samples fabricated by hot
         |""".stripMargin
    )
  )

  import smile.sequence.{CRF, crf}
  import smile.data.{Tuple, DataFrame}
  import scala.jdk.StreamConverters._
  import com.spotify.featran._
  import com.spotify.featran.transformers._
  import com.spotify.featran.converters._

  // Features for each individual "focus" shape (as defined by octothorpe)
  case class TextLineFeatures(
    b: Boolean,
    f: Double
  )
  it should "identify text lines" in {
    //
    val spec = FeatureSpec
      .of[TextLineFeatures]
      .required(_.b.asDouble)(Identity("isWhatever"))
      .required(_.f.toDouble)(Identity("goldenRatio"))

    // This corresponds to the list of shape feature vectors for a single line
    val textLineObserved: Array[TextLineFeatures] = Array(
      TextLineFeatures(true, 12.3),
      TextLineFeatures(false, 22.4),
      TextLineFeatures(true, 1.3),
      TextLineFeatures(false, 2.4),
      TextLineFeatures(false, 2.4),
      TextLineFeatures(true, 1.3)
    )

    // Use featran to generate sequences...
    val featureExtractor: FeatureExtractor[Array, TextLineFeatures] =
      spec.extract(textLineObserved)

    val featuresAsDoubles: Array[Array[Double]] =
      featureExtractor.featureValues[Array[Double]]

    val featureNames              = featureExtractor.featureNames
    val featureNames0             = featureNames.head
    val labels: Array[Array[Int]] = Array(
      Array(1, 1, 1, 0, 0, 1)
    )

    val dataFrame = DataFrame.of(featuresAsDoubles, featureNames0: _*)
    pprint.pprintln(dataFrame)

    val tupleStream: stream.Stream[Tuple] = dataFrame.data.stream()
    val tupleArray: Array[Tuple]          = tupleStream.toScala(Array)

    //
    val tupleArrays: Array[Array[Tuple]] = Array(
      tupleArray
    )

    val theCrf: CRF = crf(
      tupleArrays,
      labels,
      ntrees = 100,
      maxDepth = 20,
      maxNodes = 100,
      nodeSize = 5,
      shrinkage = 1.0
    )

    val prediction = theCrf.predict(tupleArray)
    pprint.pprintln(s"""Prediction ${prediction.mkString(", ")}""")

  }

}
