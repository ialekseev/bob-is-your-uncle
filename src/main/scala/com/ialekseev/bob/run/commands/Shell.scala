package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command
import scalaz.effect.IO

trait Shell {
  this: Command with Check with Service =>

  case class Config(check: String = "", service: Boolean = false, shell: Boolean = false, help: Boolean = false, quit: Boolean = false)
  val parser = new scopt.OptionParser[Config]("bob") {

    override def errorOnUnknownArgument: Boolean = false
    override def showUsageOnError: Boolean = false

    opt[String]("check").action((x, c) => c.copy(check = x)).text("check a file (value - is a path to the file)")
    opt[Unit]("shell").action((_, c) => c.copy(shell = true)).text("enter the shell")
    opt[Unit]("service").action((_, c) => c.copy(service = true)).text("run the http service")
    opt[Unit]("help").action((_, c) => c.copy(help = true)).text("show help")
    opt[Unit]("quit").action((_, c) => c.copy(quit = true)).text("exit the shell")
  }

  val color = Console.MAGENTA

  def shellCommand(): IO[Unit] = {

    def shell(): IO[Unit] = {
      for {
       str <- read(color + "bob> ")
       _ <- parser.parse(str.split(" +").toSeq, Config())  match {
         case Some(Config(path, _, _, _, _)) if path.nonEmpty => checkCommand(path).flatMap(_ => shell())
         case Some(Config(_, true, _, _, _)) => serviceCommand().flatMap(_ => shell())
         case Some(Config(_, _, true, _, _)) => show("you are already in the shell\n").flatMap(_ => shell())
         case Some(Config(_, _, _, true, _)) => showHelp().flatMap(_ => shell())
         case Some(Config(_, _, _, _, true)) => show("quitting...")
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
