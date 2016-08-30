package com.ialekseev.bob.run.commands

import scala.io._
import scala.util.control.Breaks._

trait Shell {
  this: Check with Service =>

  lazy val color = Console.MAGENTA

  def shellCommand() = {
    println(color)
    println("Welcome to Bob's shell. Type 'help' for information.")
    println(Console.RESET)
    breakable {
      while(true){
        StdIn.readLine(color + "bob> ").split(" +").toSeq match {
          case Seq("check", path) => checkCommand(path)
          case Seq("service") => serviceCommand()
          case Seq("help") => showHelp()
          case Seq("quit" | "exit" | ":q") => break
          case _ => {
            println(Console.RED + " Invalid command. See help:" + Console.RESET)
            showHelp()
          }
        }
      }
    }
  }

  private def showHelp() = {
    println(Console.GREEN)
    println(" ?HELP?")
    println(" [check <path>]       - check if the specified file is a correct Bob's")
    println(" [service]            - run the http service")
    println(" [quit | exit | :q]   - quit the shell")
    println(" [help]               - command listing")
    println(Console.RESET)
  }
}
