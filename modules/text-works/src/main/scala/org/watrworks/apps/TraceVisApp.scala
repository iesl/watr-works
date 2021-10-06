package org.watrworks
package apps

import Pipelines._
import utils.{M3x3Position => M3}
import scala.sys.process._
import segment.prelude._
import transcripts.Transcript
import geometry._
import geometry.syntax._
import utils.ExactFloats._

import zio._
import zio.stream._
import zio.console._
import corpora.filesys.CorpusEntry
import java.io.IOException

object TraceVis {


  def listClusterings(transcript: Transcript): Stream[Nothing, ShapeClustering.Root] = for {
    label <- Stream.fromIterable(transcript.labels)
    if label.name.endsWith("ClusteringRoot")
  } yield {
    ShapeClustering.fromTranscriptLabel(transcript, label)
  }

  type RC = (ShapeClustering.Root, ShapeClustering.ClusteredInstances)
  def chooseClustering(
    clusterRoots: UStream[ShapeClustering.Root]
  ): ZIO[Console, Any, RC] = {

    val res: ZIO[Console, Throwable, RC] = for {
      _ <- putStrLn(s"Choose Clustering")
      askUserStr <- clusterRoots.zipWithIndex
                      .map({
                        case (root, rootIndex) => {
                          val clusterStrs = root.clusters.zipWithIndex
                            .map({ case (c, cIndex) => s"   ${cIndex}. count=${c.instances.length}" })
                          s"${rootIndex}. ${root.name}" :: clusterStrs
                        }
                      })
                      .runCollect
      _            <- putStrLn(askUserStr.toList.flatten.mkString("\n"))
      userResponse <- userPrompt("Choose Clustering, Cluster#")
      _            <- putStrLn(s"${userResponse}")
      Array(s1, s2) = userResponse.split("[, ][ ]*").map(_.trim())
      i1            = s1.toLong
      i2            = s2.toInt
      _ <- putStrLn(s"Clustering ${i1}, cluster ${i2} ")
      maybeRC <- clusterRoots
                   .drop(i1)
                   .runHead
                   .map(_.map(root => (root, root.clusters(i2))))
      rc <- ZIO.fromOption(maybeRC).absorbWith(_ => new Throwable(""))
    } yield {
      rc
    }

    res
  }

  def visualizeShapeClusters(
    transcript: Transcript,
    r: ShapeClustering.Root,
    cluster: ShapeClustering.ClusteredInstances
  ): Unit = {

    val formattedCommandInput: Seq[Seq[String]] = for {
      instances <- cluster.instances.take(100)
    } yield {
      val pageNum = instances.pageNum
      val doc     = transcript.documentId
      val page    = transcript.pages(pageNum.unwrap)

      instances.shape match {
        case trapezoid: Trapezoid =>
          val tl = trapezoid.topLeft
          val tr = tl.translate(trapezoid.topWidth, 0.toFloatExact())
          val bl = trapezoid.bottomLeft
          val th = trapezoid.height()

          val br   = bl.translate(trapezoid.bottomWidth, 0.toFloatExact())
          val fmt  = (p: Point) => s"${p.x.pp()},${p.y.pp()}"
          val fmtx = (p: Point) => s"${p.x.pp()}x${p.y.pp()}"
          val path = s"${fmt(tl)} ${fmt(tr)} ${fmt(br)} ${fmt(bl)}"

          val pageGeometry = page.bounds
          val pageImage =
            s"./corpus.d/${doc.unwrap}/page-images/page-${pageNum.unwrap + 1}.opt.png"
          val geom = pageGeometry.toPoint(M3.BottomRight)

          val crop =
            s"${geom.x.pp()}x${(th + 16.toFloatExact()).pp()}+0+${(tl.y - 8.toFloatExact()).pp()}"
          List(
            "(",
            pageImage,
            "-resize",
            fmtx(geom),
            "-fill",
            "blue",
            "-draw",
            s"""fill-opacity 0.3 path 'M ${path} Z'""",
            "-crop",
            crop,
            "-resize",
            "200%",
            ")"
          )

        case _ => List()
      }
    }

    val combinedArgs = formattedCommandInput.flatten

    val cmdList = (List("convert", "-font", "ubuntu") ++ combinedArgs) ++ List("miff:-")
    val montage = "montage - -font ubuntu -bordercolor blue -background green -mattecolor black -border 1 -tile 3x -geometry +2+2 x:"
      .split(" ")
      .toList

    val _ = (cmdList #| montage).!
  }

  val userPrompt: String => ZIO[Console, Throwable, String] = (prompt: String) => {
    for {
      _      <- putStrLn(s":> ${prompt}")
      _      <- putStr(":> ")
      filter <- getStrLn
    } yield filter
  }

  val transcriptForEntry: CorpusEntry => Either[Throwable, Transcript] = (corpusEntry) => {
    val transcriptArtifact = corpusEntry.resolveArtifact("transcript.json", None)
    for {
      json       <- transcriptArtifact.asJson.toEither
      transcript <- json.as[Transcript]
    } yield transcript
  }

  val appLogic: ZIO[Console, Any, Any] = for {
    _ <- putStrLn(s"___ TraceVis ___")
    entries <- corpusEntryStream(config(".*")).runCollect
                 .absorbWith(_ => new IOException(""))
    _           <- putStrLn(s"Entries:")
    _           <- putStrLn(entries.map(e => e.entryDescriptor.split("/").last).mkString("\n"))
    filter      <- userPrompt("Filter Regex")
    _           <- putStrLn(s"Running with Filter ${filter}")
    chosenEntry <- ZIO.fromOption(entries.filter(e => e.entryDescriptor.matches(s".*${filter}.*")).headOption)
    transcript  <- ZIO.fromEither(transcriptForEntry(chosenEntry))
    _           <- putStrLn(s"Transcript:   ${transcript.documentId}")
    clusterings = listClusterings(transcript)
    (root, cluster) <- chooseClustering(clusterings)
    _ = visualizeShapeClusters(transcript, root, cluster)
  } yield ()

  def go() = {
    val runtime = Runtime.default
    runtime.unsafeRun(appLogic)
  }

}
