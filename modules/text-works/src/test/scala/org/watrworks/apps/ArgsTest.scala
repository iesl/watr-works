package org.watrworks
package apps

import scopt._
import shapeless._

trait RootT {
  def mod[C[_], A]: Lens[C[A], A] // = lens[Root.Config[A]].cmd
  // def mod = Root.mod[Config]
  // val b = Root.buildCmd[Config]; import b._

}
// trait SubCmd[RootT, CmdT] {
//   val mod = Root.mod[Config]
//   val b = Root.buildCmd[Config]; import b._

// }

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

  def mkParser[A](
    name: String,
    initConfig: A
  )(
    mk: OParser[_, Config[A]] => OParser[_, Config[A]]
    // mk: (OParser[_, Config[_]], OParserBuilder[Config[A]], Lens[Config[A], A]) => OParser[_, Config[_]]
  ): OParser[Unit, Config[_]] = {
    val bcmd = buildCmd[A]
    val initSeq: OParser[_, Config[A]] = OParser.sequence(
      bcmd
        .cmd(name)
        .action((_, c) => c.copy(cmd = initConfig))
    )

    val ps = mk(initSeq)
    ps.asInstanceOf[OParser[Unit, Config[_]]]

    // ps match {
    //   case Nil => ???
    //   case List(phead) =>
    //     OParser
    //       .sequence(
    //         cmd(name)
    //           .action((_, c) => c.copy(cmd = Config()))
    //           .children(
    //             phead
    //           )
    //       )
    //       .asInstanceOf[OParser[Unit, Config[_]]]
    //   case p0 +: ptail =>
    //     OParser
    //       .sequence(
    //         p0,
    //         ptail: _*
    //       )
    //       .asInstanceOf[OParser[Unit, Config[_]]]
    // }
    // ???
  }

  def mkParser2[A](
    mk: (OParserBuilder[Config[A]], Lens[Config[A], A]) => (
      OParser[Unit, Config[A]],
      Seq[OParser[_, Config[A]]]
    )
  ): OParser[Unit, Config[_]] = {
    val (p0, ptail) = mk(buildCmd[A], mod)
    OParser
      .sequence(
        p0,
        ptail: _*
      )
      .asInstanceOf[OParser[Unit, Config[_]]]
    // ???
  }

}
object CmdA {
  case class Config(
    verbose: Boolean = false
  )

  val parser = Root.mkParser[Config]("cmd-a", Config()) { case (initCmd) =>
    // import bld._
    val update = Root.mod[Config]
    val b      = Root.buildCmd[Config]; import b._

    initCmd.children(
      opt[Boolean]("verbose")
        .text("verbosity level")
        .action((value, c) => update.verbose.modify(c)(_ => value))
    )
  }

  // val mod = Root.mod[Config]
  // val b   = Root.buildCmd[Config]; import b._

  // val parser = OParser
  //   .sequence(
  //     cmd("cmd-a")
  //       .action((_, c) => c.copy(cmd = Config()))
  //       .children(
  //         opt[Boolean]("verbose")
  //           .text("verbosity level")
  //           .action((value, c) => mod.verbose.modify(c)(_ => value))
  //       )
  //   )
  //   .asInstanceOf[OParser[Unit, Root.Config[_]]]
}

object CmdB {
  case class Config(
    filepath: String = ""
  )

  val mod = Root.mod[Config]
  val b   = Root.buildCmd[Config]; import b._

  val parser: OParser[Unit, Root.Config[_]] = OParser
    .sequence(
      cmd("cmd-b")
        .action((_, c) => c.copy(cmd = Config()))
        .children(
          opt[String]("file")
            .text("file path")
            .action((value, c) => mod.filepath.modify(c)(_ => value))
        )
    )
    .asInstanceOf[OParser[Unit, Root.Config[_]]]
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
      List("cmd-b", "--file", "/some/file/path"),
      List("bogus")
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

  // it should "factor out subcommands" in {

  //   val builder = OParser.builder[RootConfig[Any]]
  //   import builder._

  //   val parser = OParser.sequence(
  //     programName("MyApp"),
  //     head("MyApp is This", "version 1.0"),
  //     cmd("cmd-a")
  //       .action((_, c) => c.copy(commandConfig = CmdAConfig()))
  //       .children(
  //         opt[Boolean]("verbose")
  //           .text("verbosity level")
  //           // .action((_, c) => c.copy( = false))
  //       ),
  //     cmd("cmd-b")
  //       .action((_, c) => c.copy(commandConfig = CmdBConfig())),
  //   )

  //   val args = List("cmd-a")
  //   val init = RootConfig[Any]()
  //   val result = OParser.parse(parser, args, init)

  //   pprint.pprintln(result)
  // }
}
