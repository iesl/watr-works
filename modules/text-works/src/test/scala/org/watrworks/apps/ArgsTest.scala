package org.watrworks
package apps

import scopt._
import shapeless._
import scala.util.matching.Regex

object Args {
  def embedConfig[ParentC, ChildC](
    childC: => ChildC,
    setter: (ParentC, ChildC) => ParentC,
    mk: => OParser[_, ChildC]
  ): OParser[Unit, ParentC] = {
    // val childParser = mk
    // setter()
    ???
  }

  def embedCmd[ParentC, ChildC](
    name: String,
    initConfig: => ChildC
  )(
    mk: OParser[_, ChildC] => OParser[_, ChildC],
    // view: Lens[ParentC, ChildC]
    embedChild: ChildC => ChildC
  ): OParser[Unit, ParentC] = {
    val b = OParser.builder[ChildC]
    val initSeq: OParser[_, ChildC] = OParser.sequence(
      b.cmd(name)
        .action((value, c) => embedChild(initConfig))
    )

    val ps = mk(initSeq)
    ps.asInstanceOf[OParser[Unit, ParentC]]
  }

}

object Root {
  case class Config[+A](
    cmd: A = ()
  )

  def mod[A]: Lens[Config[A], A] = lens[Config[A]].cmd

  def buildCmd[A] = OParser.builder[Config[A]]

  val builder = OParser.builder[Config[_]]
  import builder._

  val parser = OParser
    .sequence(
      programName("TextWorks"),
      head("TextWorks")
    )

  def parseCmd[A](name: String, initConfig: => A)(
    mk: OParser[_, Config[A]] => OParser[_, Config[A]]
  ): OParser[Unit, Config[_]] = {
    val bcmd = buildCmd[A]
    val initSeq: OParser[_, Config[A]] = OParser.sequence(
      bcmd
        .cmd(name)
        .action((_, c) => c.copy(cmd = initConfig))
    )

    val ps = mk(initSeq)
    ps.asInstanceOf[OParser[Unit, Config[_]]]
  }

}

object GenOpts {

  // def init(): OParser[Unit, HNil] = {
  //   val bopt = OParser.builder[HNil]

  //   val wer: OParser[Unit, HNil] = bopt
  //     .action((value, c) => HNil)

  //   wer
  // }

  def boolX(): OParser[Boolean, Boolean] = {
    val bopt = OParser.builder[Boolean]

    val wer: OParser[Boolean, Boolean] = bopt
      .opt[Boolean]("overwrite")
      .text("overwrite existing files")
      // .action((value, c) => c)

    wer
  }

  def boolU[T <: HList](): OParser[Unit, Boolean :: T] = {
    val bopt = OParser.builder[Boolean :: T]


    val wer: OParser[Unit, Boolean :: HNil] = bopt
      .opt[Boolean]("overwrite")
      .text("overwrite existing files")
      .action((value, c) => value :: HNil)

    wer
  }

  def bool[H <: Boolean, T <: HList](): OParser[Boolean, H :: T] = {
    val bopt = OParser.builder[H :: T]

    val wer: OParser[Boolean, H :: T] = bopt
      .opt[Boolean]("overwrite")
      .text("overwrite existing files")
      .action((value, c) => c)

    wer
  }

  def regex[H <: String, T <: HList](): OParser[String, H :: T] = {
    val bopt = OParser.builder[H :: T]

    val wer: OParser[String, H :: T] = bopt
      .opt[String]("filter")
      .text("path regex for filtering")
      .action((value, c) => c)

    wer
  }


  def optParser0(): OParser[Unit, HList] = OParser.sequence(
    bool[Boolean, HNil](),
    // regex[String, Boolean :: HNil]()
  )

  val optParser1 = for {
    b0   <- boolX()
    b1   <- boolX().validate(_ => Right((): Unit))
    // re0 <- regex()
  } yield ()

}

object CmdA0 {}

object CmdA {
  case class Config(
    verbose: Boolean = false
  )

  val update = Root.mod[Config]
  val b      = Root.buildCmd[Config]; import b._

  val parser = Root.parseCmd[Config]("cmd-a", Config()) { case (initCmd) =>
    initCmd.children(
      opt[Boolean]("verbose")
        .text("verbosity level")
        .action((value, c) => update.verbose.modify(c)(_ => value))
    )
  }
}

object CmdB {

  case class Config(
    filepath: String = "",
    filters: Filtering.Config = Filtering.Config()
  )

  val update = Root.mod[Config]
  val b      = Root.buildCmd[Config]; import b._

  // val filterConf: OParser[Unit, Root.Config[Filtering.Config]] =
  //   Filtering.parser[Config]((conf, c0) => conf.copy(filters = c0))

  val parser = Root.parseCmd[Config]("cmd-b", Config()) { case (initCmd) =>
    initCmd.children(
      opt[String]("file")
        .text("file path")
        .action((value, c) => update.filepath.modify(c)(_ => value))
    )
  }
}

object Filtering {
  case class Config(
    pathFilter: Option[String] = None,
    numToRun: Int = Int.MaxValue,
    numToSkip: Int = 0
  )

  val update = lens[Config]
  // val b      = Root.buildCmd[Config]; import b._
  val b = OParser.builder[Config]
  import b._

  def validateRegex(str: String): Either[String, Unit] = try {
    new Regex(str)
    Right((): Unit)
  } catch {
    case err: Error =>
      Left(err.getMessage())
  }

  val optParser: OParser[String, Config] = OParser.sequence(
    opt[String]("filter")
      .validate(validateRegex(_))
      .action((value, c) => update.pathFilter.modify(c)(_ => Some(value))),
    opt[Int]("num-to-run")
      .action((value, c) => update.numToRun.modify(c)(_ => value)),
    opt[Int]("num-to-skip")
      .action((value, c) => update.numToSkip.modify(c)(_ => value))
  )

  def embed[ParentC](setter: (ParentC, Config) => ParentC): OParser[Unit, Config] = {

    // Args.embedConfig[ParentC, Config](
    //   Config(),
    //   setter,
    //   optParser
    // )

    ???
  }
}

class ArgsTest extends utils.SimpleTest {
  behavior of "Argument parsers"

  it should "parse subcommands" in {

    val c0 = OParser.builder[Root.Config[Any]]
    val headParser = OParser.sequence(
      c0.programName("MyApp"),
      c0.head("MyApp is This", "version 1.0")
    )

    val finalParser = OParser.sequence(
      headParser,
      CmdA.parser,
      CmdB.parser
    )
    val examples = List(
      List("cmd-a", "--verbose", "true"),
      List("cmd-b", "--file", "/some/file/path", "--filter", "zz.*")
      // List("bogus")
    )

    for {
      example <- examples
    } {
      val init   = Root.Config[Any]()
      val result = OParser.parse(finalParser, example, init)

      result.foreach {
        _.cmd match {
          case v: CmdA.Config =>
            println(s"Command A!  $v")

          case v: CmdB.Config =>
            println(s"Command B!  $v")

          case x =>
            println(s"huh? $x")

        }
      }
      pprint.pprintln(s"args = ${example}")
      pprint.pprintln(result)
    }
  }

}
