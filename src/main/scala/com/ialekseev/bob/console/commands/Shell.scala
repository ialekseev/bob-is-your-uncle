package com.ialekseev.bob.console.commands

import scala.io._
import scala.util.control.Breaks._

trait Shell {
  this: Check =>

  case class ShellConfig(check: String = "", quit: Boolean = false)
  //todo: using scopt's 'arguments'?
  val shellParser = new scopt.OptionParser[ShellConfig]("bob's shell") {
    override def terminate(exitState: Either[String, Unit]): Unit = ()

    opt[String]("check").action((x, c) => c.copy(check = x)).text("[<value> is a path to file]")
    opt[Unit]("quit").action((_, c) => c.copy(quit = true)).text("quit the shell")
  }

  def shellCommand() = {
    println("[Bob's shell]")
    println()
    breakable {
      while(true){
        val read = StdIn.readLine()
        val readPrefixed = if (read.startsWith("--")) read else "--" + read
        //todo: how to convert 'read' to args to be fed to 'scopt'
        shellParser.parse(Seq(readPrefixed), ShellConfig()) match {
          case Some(ShellConfig(path, _)) if path.nonEmpty => checkCommand(path)
          case Some(ShellConfig(_, true)) => break
          case None =>
        }
      }
    }
  }
}
