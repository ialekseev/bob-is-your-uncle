package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Shell {
  this: Command with Check with Service =>

  case class Config(shell: Boolean = false, check: Boolean = false, service: Boolean = false, help: Boolean = false, quit: Boolean = false, arguments: Arguments = Arguments(path = none))
  case class Arguments(path: Option[String] = none)

  val parser = new scopt.OptionParser[Config]("bob") {

    override def errorOnUnknownArgument: Boolean = false
    override def showUsageOnError: Boolean = false

    cmd("shell").action((_, c) => c.copy(shell = true)).text("[enter the shell]")
    note("\n")
    cmd("check").action((x, c) => c.copy(check = true)).text("[check a file]").
      children(
        opt[String]("path").action((x, c) => c.copy(arguments = Arguments(path = some(x)))).text("path to the file with bob-file")
      )
    note("\n")
    cmd("service").action((x, c) => c.copy(service = true)).text("[run the http service]").
      children(
        opt[String]("path").action((x, c) => c.copy(arguments = Arguments(path = some(x)))).text("path to the folder with more bob-files")
      )
    note("\n")
    cmd("help").optional().action((_, c) => c.copy(help = true)).text("[show help]")
    note("\n")
    cmd("quit").action((_, c) => c.copy(quit = true)).text("[exit the shell]")
  }

  val color = Console.MAGENTA

  def shellCommand(): IO[Unit] = {

    def shell(): IO[Unit] = {
      for {
       str <- read(color + "bob> ")
       _ <- parser.parse(str.split(" +").toSeq, Config())  match {
         case Some(Config(true, _, _, _, _, _)) => show("you are already in the shell\n").flatMap(_ => shell())
         case Some(Config(_, true, _, _, _, Arguments(Some(path)))) if path.nonEmpty => checkCommand(path).flatMap(_ => shell())
         case Some(Config(_, _ , true, _, _, Arguments(path))) => serviceCommand(path.toList).flatMap(_ => shell())
         case Some(Config(_, _, _, true, _, _)) => showHelp().flatMap(_ => shell())
         case Some(Config(_, _, _, _, true, _)) => show("quitting...")
         case _ => showHelp().flatMap(_ => shell())
       }
      } yield ()
    }

    for {
      _ <- show(color)
      _ <- show("Welcome to Bob's shell. Type 'help' for information.")
      _ <- show(Console.RESET)
      _ <- shell()
    } yield ()
  }

  def showHelp(): IO[Unit] = {
    for {
      _ <- show(Console.GREEN)
      _ <- IO(parser.showUsage())
      _ <- show(Console.RESET)
    } yield ()
  }
}
