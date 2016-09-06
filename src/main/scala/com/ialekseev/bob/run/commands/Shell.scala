package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command
import scala.util.control.Breaks._

trait Shell {
  this: Command with Check with Service =>

  lazy val color = Console.MAGENTA

  def shellCommand() = {
    show(color)
    show("Welcome to Bob's shell. Type 'help' for information.")
    show(Console.RESET)
    breakable {
      while(true){
        read(color + "bob> ").split(" +").toSeq match {
          case Seq("check", path) => checkCommand(path)
          case Seq("service") => serviceCommand()
          case Seq("help") => showHelp()
          case Seq("quit" | "exit" | ":q") => break
          case _ => {
            show(Console.RED + " Invalid command. See help:" + Console.RESET)
            showHelp()
          }
        }
      }
    }
  }

  private def showHelp() = {
    show(Console.GREEN)
    show(" ?HELP?")
    show(" [check <path>]       - check if the specified file is a correct Bob's")
    show(" [service]            - run the http service")
    show(" [quit | exit | :q]   - quit the shell")
    show(" [help]               - command listing")
    show(Console.RESET)
  }
}
