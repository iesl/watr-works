package org.watrworks
package segment

import org.watrworks.geometry._
import com.spotify.featran._
import com.spotify.featran.transformers._
// import com.spotify.featran.converters._

import org.watrworks.utils.ExactFloats._

import smile.clustering.{xmeans, XMeans, CentroidClustering}
import TypeTags._


class TrapezoidFindingTests extends SegmentationTest {
    behavior of "Trapezoid Clustering"

    import TrapezoidFeatures._

    it should "smokescreen" in {
        val tl = Point.Ints(1, 1)
        val bl = Point.Ints(2, 2)
        val trap = Trapezoid(tl, 10.toFloatExact(), bl, 10.toFloatExact())

        val records = List(
            TrapezoidFeatureRec(
                trap
            )
        )

        // Extract features from `Seq[Record]`
        val f1: FeatureExtractor[List, TrapezoidFeatureRec] = spec.extract(records)

        // Extract feature names and values as `Seq[Double]`
        val fnames = f1.featureNames.map(_.mkString(", ")).mkString("\n")
        println(fnames)

        val doubleS = f1.featureValues[Array[Double]].toArray
        println("Seq[Double]")
        pprint.pprintln(doubleS)
        val pp = trap.prettyPrint()
        println(s"Trapezoid = ${pp}")
        val data = doubleS

        // xmeans
        val sdf: XMeans = xmeans(data, 4)

    }


    import ammonite.{ops => fs}, fs._

    it should "extract" in {
        val path =fs.pwd / "corpus.d" / "cmp-lg9503025.pdf.d" / "cmp-lg9503025.pdf"

        val docId = DocumentID("todo")
        val segmenter = DocumentSegmenter.createSegmenter(docId, path)

        segmenter.runDocumentSegmentation()
    }
}
