package org.watrworks
package apps

import ammonite.{ops => fs} // , fs._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import tracing.VisualTracer
import utils.PathUtils._
import utils.TextOps._
import ProcessPipelineSteps._
import utils.{RelativeDirection => Dir}
import scala.sys.process._
import cats.effect._
import segment._
import transcripts.Transcript
import geometry._
import geometry.syntax._
import utils.ExactFloats._

object TraceVisConfig {

  implicit val NioPath: Read[nio.Path] =
    Read.reads { v =>
      nio.Paths.get(v).toAbsolutePath().normalize()
    }

  case class Config(
    ioConfig: IOConfig = IOConfig(),
    initCorpus: Option[nio.Path] = None,
    runTraceLogging: Boolean = VisualTracer.tracingEnabled()
  )

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    help("help")

    opt[nio.Path]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.modify(conf) { ioConfig =>
        ioConfig.copy(
          inputMode = Option(Processable.CorpusRoot(v))
        )
      }
    } text ("root path of PDF corpus")

    opt[String]("filter") action { (v, conf) =>
      lens[Config].ioConfig.pathFilter.modify(conf) { m =>
        val f = trimQuotes(v)
        Option(f)
      }
    } text ("if specified, only files matching regex will be processed")

    checkConfig { c =>
      if (c.initCorpus.isDefined) {
        val corpusRoot = c.initCorpus.get
        if (fs.exists(corpusRoot.toFsPath())) {
          success
        } else {
          failure(s"Corpus root ${corpusRoot} doesn't exist")
        }
      } else {
        c.ioConfig.inputMode.map {
          _ match {
            case Processable.CorpusRoot(rootPath) =>
              if (fs.exists(rootPath.toFsPath())) success
              else failure(s"corpus root ${rootPath} not found")
            case _                                => ???

          }
        } getOrElse {
          failure("No input specified")
        }
      }
    }
  }

}

// import transcripts.Transcript
// import geometry._
// import geometry.syntax._
// import utils.ExactFloats._
// import textboxing.{TextBoxing => TB}, TB._

object TraceVis {
  def config(filter: String): TraceVisConfig.Config = {
    val config = TraceVisConfig.Config(
      IOConfig(
        inputMode = Option(
          Processable.CorpusRoot(
            nio.Paths.get("corpus.d").toAbsolutePath().normalize()
          )
        ),
        pathFilter = Some(filter)
      )
    )
    config
  }

  def selectTranscripts(implicit conf: TraceVisConfig.Config): List[Transcript] = {
    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(dropSkipAndRun(conf.ioConfig))

    val prog = processStream.compile.toList

    val inputs      = prog.unsafeRunSync()
    println(s"inputs: ${inputs}")
    val transcripts = inputs.flatMap {
      case Right(Processable.CorpusFile(corpusEntry)) =>
        val transcriptArtifact = corpusEntry.resolveArtifact("transcript.json", None)
        transcriptArtifact.asJson.toEither match {
          case Left(x) =>
            println(s"Error: ${x}")
            List()

          case Right(json) =>
            val mtrans = json.as[Transcript]
            mtrans match {
              case Left(err)         =>
                println(s"Error: ${err}")
                List()
              case Right(transcript) => List(transcript)
            }
        }

      case x =>
        println(s"Left entry: ${x}")
        List()
    }
    transcripts
  }

  def listClusterings(transcript: Transcript): Seq[ShapeClustering.Root] = for {
    label <- transcript.labels
    if label.name.endsWith("ClusteringRoot")
  } yield {
    ShapeClustering.fromTranscriptLabel(transcript, label)
  }

  def chooseClustering(
    clusterings: Seq[ShapeClustering.Root]
  ): (ShapeClustering.Root, ShapeClustering.ClusteredInstances) = {
    import scala.io.StdIn.readLine
    println(s"Choose Clustering")
    for {
      (clustering, clusteringNum) <- clusterings.zipWithIndex
      clusteringName = clustering.name
      _              = println(s"  ${clusteringNum}. ${clusteringName}")
      (cluster, clusterNum) <- clustering.clusters.zipWithIndex
      _ = println(s"     ${clusterNum} size = ${cluster.instances.length}")
      // instances <- cluster.instances
    } yield ()

    println(s"Clustering, Cluster numbers (3, 23) ? > ")
    val in            = readLine()
    val Array(s1, s2) = in.split("[, ][ ]*").map(_.trim())
    val i1            = s1.toInt
    val i2            = s2.toInt
    println(s"Clustering ${i1}, cluster ${i2} ")

    val root    = clusterings(i1)
    val cluster = root.clusters(i2)
    (root, cluster)
  }

  def visualizeShapeClusters(
    transcript: Transcript,
    r: ShapeClustering.Root,
    cluster: ShapeClustering.ClusteredInstances
  ): Unit = {

    val formattedCommandInput: Seq[Seq[String]] = for {
      instances <- cluster.instances
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
          val pageImage    =
            s"./corpus.d/${doc.unwrap}/page-images/page-${pageNum.unwrap + 1}.opt.png"
          val geom         = pageGeometry.toPoint(Dir.BottomRight)

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
    val montage = List(
      "montage",
      "-",
      "-font",
      "ubuntu",
      "-bordercolor",
      "blue",
      "-border",
      "1",
      "-tile",
      "3x",
      "-geometry",
      "+2+2",
      "x:"
    )

    // println(s"running...${cmdList} => ${montage}")
    val _ = (cmdList #| montage).!
  }

  def run(): Unit = {
    println("Running")
    implicit val initConfig = config(".*aust.*")
    val initTranscripts     = selectTranscripts(initConfig)
    if (initTranscripts.length > 0) {
      println(s"Transcript Count = ${initTranscripts.length}")

      val headTranscript   = initTranscripts.head
      val clusterings      = listClusterings(headTranscript)
      val chosenClustering = chooseClustering(clusterings);
      val (root, cluster)  = chosenClustering;
      visualizeShapeClusters(headTranscript, root, cluster)
    }

  }

}
