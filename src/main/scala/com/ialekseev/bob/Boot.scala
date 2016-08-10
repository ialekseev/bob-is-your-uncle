package com.ialekseev.bob

import com.ialekseev.bob.console.commands.{ShellCommand, CheckCommand}
import com.ialekseev.bob.console.{Command, BobConsole}

object Boot extends App with Command with BobConsole with CheckCommand with ShellCommand {

  case class Config(check: String = "")
  val parser = new scopt.OptionParser[Config]("bob") {
    opt[String]("check").action( (x, c) =>
      c.copy(check = x) ).text("check - is a path to file")
  }

  parser.parse(args, Config()) match {
    case Some(Config(path)) if path.nonEmpty => checkCommand(path)
    case None =>
  }
}
