package org.watrworks
package segment

import org.scalatest.diagrams.Diagrams
import java.util.stream

class LineTokenizationTest extends SegmentationTestUtils with Diagrams {
  behavior of "text line identification"

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
  it should "create sample pdf data" in {
    // VisualLine = L
    // Baseline   = B
    // SupScript = T
    // SubScript = V
    // StackedScript H


    """|Here s is the item indexed array for the successful responses r_{i->j}^{[k]} at time point k.
       |""".stripMargin

    """|
       |v |Here
       |v |$s$
       |v |is the item indexed array for the successful responses
       |v |$r_{i\rightarrow j}^{[k]}$
       |v |at time point
       |v |$k$.
       |""".stripMargin

  }
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
