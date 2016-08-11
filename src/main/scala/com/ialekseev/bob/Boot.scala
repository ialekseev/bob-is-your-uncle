package com.ialekseev.bob

import com.ialekseev.bob.console.commands.{Shell, Check}
import com.ialekseev.bob.console.{Command}

object Boot extends App with Command with Check with Shell {

  case class Config(check: String = "", shell: Boolean = false)
  val parser = new scopt.OptionParser[Config]("bob") {
    opt[String]("check").action((x, c) => c.copy(check = x)).text("check - is a path to file")
    opt[Unit]("shell").action((_, c) => c.copy(shell = true)).text("shell - enter the shell")
  }

  parser.parse(args, Config()) match {
    case Some(Config(path, _)) if path.nonEmpty => checkCommand(path)
    case Some(Config(_, true)) => shellCommand()
    case None =>
  }
}
