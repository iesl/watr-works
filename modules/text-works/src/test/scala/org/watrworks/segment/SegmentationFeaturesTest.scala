package org.watrworks
package segment

import com.spotify.featran._
import com.spotify.featran.transformers._

// Import Featran core, transformers and `asDouble` converter for `Boolean`
import com.spotify.featran.converters._
import breeze.linalg._

class SegmentationFeaturesTest extends utils.SegmentationTestUtils {
  // Input record type
  case class Record(
    b: Boolean,
    f: Float,
    d1: Double,
    d2: Option[Double],
    d3: Double,
    s1: String,
    s2: List[String])

  // # Random input data
  import org.scalacheck._
  val recordGen: Gen[Record] = for {
    b <- Arbitrary.arbitrary[Boolean]
    f <- Arbitrary.arbitrary[Float]
    d1 <- Arbitrary.arbitrary[Double]
    d2 <- Arbitrary.arbitrary[Option[Double]]
    d3 <- Gen.choose(0, 24)
    s1 <- Gen.alphaStr.map(_.take(5))
    n <- Gen.choose(0, 10)
    s2 <- Gen.listOfN(n, Gen.alphaStr.map(_.take(5)))
  } yield Record(b, f, d1, d2, d3.toDouble, s1, s2)


  // Method for extracting an `Array[Double]` from a `Record`
  def toArray(r: Record): Array[Double] =
    Array(r.b.asDouble, r.f.toDouble, r.d1, r.d2.getOrElse(0.0))

  // Random generator for `Seq[Record]`
  val recordsGen: Gen[List[Record]] = Gen.listOfN(3, recordGen)

  it should "feature specs #one" in {
    // Random input records
    val records = recordsGen.sample.get

    // # Feature specification

    // A `FeatureSpec` defines the record type to extract features from plus a set of required or
    // optional field extractors and corresponding transformers.

    // Start a specification for input record type `Record`
    val spec = FeatureSpec
      .of[Record]
      // Required field with `Boolean`/`Float` to `Double` conversion, pass value through with
      // Identity transformer
      .required(_.b.asDouble)(Identity("id1"))
      .required(_.f.toDouble)(Identity("id2"))

    // Extract features from `Seq[Record]`
    val f1: FeatureExtractor[List, Record] = spec.extract(records)

    // Extract feature names and values as `Seq[Double]`
    // val fnames = f1.featureNames.map(_.mkString(", ")).mkString("\n")

    // Get feature values in different output types
    val doubleS = f1.featureValues[Seq[Double]]
    println("Seq[Double]")
    pprint.pprintln(doubleS)

    val floatA = f1.featureValues[Array[Float]]
    println("Array[Float]")
    pprint.pprintln(floatA)
    // val doubleA = f1.featureValues[Array[Double]]

    val floatDV = f1.featureValues[DenseVector[Float]]
    println("DenseVector[Float]")
    pprint.pprintln(floatDV)
    // val doubleDV = f1.featureValues[DenseVector[Double]]
    val floatSV = f1.featureValues[SparseVector[Float]]
    println("SparseVector[Float]")
    pprint.pprintln(floatSV)
    // val doubleSV = f1.featureValues[SparseVector[Double]]



    // Get feature values as above with rejections and the original input record
    println("Results")
    val doubleAResults = f1.featureResults[Array[Double]]

    println("Results: A[D]")
    pprint.pprintln(doubleAResults)

    val doubleAValues = doubleAResults.map(_.value)
    println("Results: A[V]")
    pprint.pprintln(doubleAValues)

    val doubleARejections = doubleAResults.map(_.rejections)
    println("Rejections")
    pprint.pprintln(doubleARejections)

  }

  it should "demo all feature specs" in {

    // Random input records
    val records = recordsGen.sample.get

    // # Feature specification

    // A `FeatureSpec` defines the record type to extract features from plus a set of required or
    // optional field extractors and corresponding transformers.

    // Start a specification for input record type `Record`
    val spec = FeatureSpec
      .of[Record]
      // Required field with `Boolean`/`Float` to `Double` conversion, pass value through with
      // Identity transformer
      .required(_.b.asDouble)(Identity("id1"))
      .required(_.f.toDouble)(Identity("id2"))

      // Pass through with Vector Identity
      .required(v => Seq(v.f.toDouble))(VectorIdentity("vec_id", 1))

      // Binarize with default threshold 0.0 and with custom threshold
      .required(_.d1)(Binarizer("bin1"))
      .required(_.d1)(Binarizer("bin2", threshold = 0.5))

      // Optional field with default missing value `None` and custom missing value
      // Bucketize into 3 bins
      .optional(_.d2)(Bucketizer("bucket1", Array(0.0, 10.0, 20.0, 30.0)))
      .optional(_.d2, Some(10.0))(Bucketizer("bucket2", Array(0.0, 10.0, 20.0, 30.0)))

      // Scale by absolute max value, scale between [0.0, 1.0] and scale between custom min and
      // max
      .required(_.d1)(MaxAbsScaler("abs"))
      .required(_.d1)(MinMaxScaler("min_max1"))
      .required(_.d1)(MinMaxScaler("min_max2", 0.0, 100.0))

      // Evaluate von Mises distribution (mu = d3, kappa = 2) at values 0, 4, .., 24
      // (rescaled using scale=pi/12 to be in the interval [0, 2pi])
      .required(_.d3)(
        VonMisesEvaluator(
          "von_mises",
          2.0,
          math.Pi / 12,
          Array(0.0, 4.0, 8.0, 12.0, 16.0, 20.0, 24.0)
        )
      )

      // Normalize vector with default `p` 2.0 and custom `p`
      .required(toArray)(Normalizer("norm1"))
      .required(toArray)(Normalizer("norm2", 3.0))

      // One hot, n-hot and n-hot weighted encoders
      .required(_.s1)(OneHotEncoder("one_hot"))
      .required(_.s1)(PositionEncoder("position"))
      .required(_.s2)(NHotEncoder("n_hot"))
      .required(_.s2.map(s => WeightedLabel(s, 0.5)))(NHotWeightedEncoder("n_hot_weighted"))

      // Encoders that hash features into buckets to reduce CPU and memory overhead
      .required(_.s1)(HashOneHotEncoder("hash_one_hot"))
      .required(_.s2)(HashNHotEncoder("hash_n_hot"))
      .required(_.s2.map(s => WeightedLabel(s, 0.5)))(
        HashNHotWeightedEncoder("hash_n_hot_weighted")
      )

      // `Record` to `Array[Double]` composite feature, polynomial expansion with default degree 2
      // and custom degree
      .required(toArray)(PolynomialExpansion("poly1"))
      .required(toArray)(PolynomialExpansion("poly2", 3))

      // Transform to absolute value first since QuantileDiscretizer requires non-negative value,
      // discretize into 4 quantiles
      .required(x => math.abs(x.d1))(QuantileDiscretizer("quantile", 4))

      // Standard score with default withStd = true, withMean = false and with custom settings
      .required(_.d1)(StandardScaler("std1"))
      .required(_.d1)(StandardScaler("std2", withStd = false, withMean = true))

    // # Feature extraction

    // A `FeatureExtractor` defines the execution logic to extract feature names, values and
    // settings from an input collection of records.

    // Extract features from `Seq[Record]`
    val f1: FeatureExtractor[List, Record] = spec.extract(records)

    // Extract feature names and values as `Seq[Double]`
    println(f1.featureNames.head)
    f1.featureValues[Seq[Double]].foreach(println)

    // Get feature values in different output types
    // val floatA = f1.featureValues[Array[Float]]
    // val doubleA = f1.featureValues[Array[Double]]

    // val floatDV = f1.featureValues[DenseVector[Float]]
    // val doubleDV = f1.featureValues[DenseVector[Double]]
    // val floatSV = f1.featureValues[SparseVector[Float]]
    // val doubleSV = f1.featureValues[SparseVector[Double]]

    // Get feature values as above with rejections and the original input record
    // val doubleAResults = f1.featureResults[Array[Double]]
    // val doubleAValues = doubleAResults.map(_.value)
    // val doubleARejections = doubleAResults.map(_.rejections)
    // val doubleAOriginals = doubleAResults.map(_.original)

    // # Feature settings

    // Feature settings is a JSON string representing aggregated global summary of a dataset, used
    // by some transformers to transform individual values. In batch pipelines, computing settings
    // requires a `reduce` step over the entire dataset. For cases with unbounded input, e.g.
    // streaming pipelines or backend services, computing settings may not be feasible and it's
    // necessary to use a previous saved settings.

    // Extract settings as a JSON string
    val settings = f1.featureSettings
    println(settings.head)

    // Extract features from new records, but reuse previously saved settings. This bypasses
    // the reduce step that aggregates feature settings from the dataset.
    // val f2 = spec.extractWithSettings(recordsGen.sample.get, settings)

    // Filter out results with rejections and extract valid values
    // val validValues = f2.featureResults[Seq[Double]].filter(_.rejections.isEmpty).map(_.value)

    // # Extract from an unbounded source

    // The record extractor is more efficient and recommended when extracting features from an
    // unbounded source, e.g. a stream of events or a backend service. This extractor parses the
    // settings only once and can process one input record at a time.
    val recordExtractor = spec.extractWithSettings[Seq[Double]](settings.head)

    // Extract from a single record
    recordExtractor.featureResult(recordGen.sample.get)

  }
}
