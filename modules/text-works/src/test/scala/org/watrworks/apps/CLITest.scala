package org.watrworks
package apps

import java.nio.file.{Path => JPath}

import com.monovore.decline._
import cats.implicits._
// import cats.data.Validated

sealed trait Gitcommand extends Product with Serializable
object Gitcommand {
  final case class Add(modified: Boolean, directory: JPath) extends Gitcommand
  final case class Remote(verbose: Boolean)                 extends Gitcommand

}

class CLITest extends utils.SimpleTest {
  behavior of "Command-line arg parsers"

  val modifiedFlag: Opts[Boolean] = Opts.flag("modified", short = "m", help = "modified?").orFalse

  val verboseFlag  = Opts.flag("verbose", short = "v", help = "Verbose output").orFalse

  val file: Opts[JPath] = Opts.argument[JPath](metavar = "file")

  val pathArg = Opts.argument[JPath]("path")

  Opts
    .option[JPath]("corpus-root", help = "Root of existing corpus")
    .validate("Directory must exists and be a valid corpus structure")(p => true) // TODO check validity

  val add: Command[Gitcommand.Add] = Command(
    name = "add",
    header = "Add unstaged items to git"
  ) {
    (modifiedFlag, file).mapN(Gitcommand.Add)
  }

  val remote: Command[Gitcommand.Remote] = Command(name = "remote", header = "list remote repository") {
    verboseFlag.map(Gitcommand.Remote)
  }

  val git: Command[Gitcommand] =
    Command(
      name = "git",
      header = "This is fake git") {
      Opts.subcommands(add, remote)
    }

  it should "smokescreen" in {
    val examples = List(
      List("git", "add", "-m", "my/dir")
    )
    for {
      args: List[String] <- examples
    } {

      val parsed: Either[Help,Any] = git.parse(args)

      parsed match {
        case Left(validationError) =>
          println(s"failure: ${validationError}")


        case Right(cmd) =>
          println(s"success: ${cmd}")

          "Success"
      }
    }
  }
}
