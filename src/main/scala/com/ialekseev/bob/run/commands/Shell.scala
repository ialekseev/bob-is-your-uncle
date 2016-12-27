package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command

import scalaz.effect.IO

trait Shell {
  this: Command with Check with Service =>

  lazy val color = Console.MAGENTA

  def shellCommand(): IO[Unit] = {

    def shell(): IO[Unit] = { //todo: trampoline?
      for {
       str <- read(color + "bob> ")
       _ <- str.split(" +").toSeq match {
         case Seq("check", path) => checkCommand(path).flatMap(_ => shell())
         case Seq("service") => serviceCommand().flatMap(_ => shell())
         case Seq("help") => showHelp().flatMap(_ => shell())
         case Seq("quit" | "exit" | ":q") => show("quitting...")
         case _ => show(Console.RED + " Invalid command. See help:" + Console.RESET).flatMap(_ => showHelp())
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

  private def showHelp(): IO[Unit] = {
    for {
      _ <- show(Console.GREEN)
      _ <- show(" ?HELP?")
      _ <- show(" [check <path>]       - check if the specified file is a correct Bob's")
      _ <- show(" [service]            - run the http service")
      _ <- show(" [quit | exit | :q]   - quit the shell")
      _ <- show(" [help]               - command listing")
      _ <- show(Console.RESET)
    } yield ()
  }
}
