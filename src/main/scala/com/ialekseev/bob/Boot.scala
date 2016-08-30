package com.ialekseev.bob

import com.ialekseev.bob.run.commands.{Service, Shell, Check}
import com.ialekseev.bob.run.{Command}

object Boot extends App with Command with Check with Shell with Service {

  case class Config(check: String = "", shell: Boolean = false, service: Boolean = false)
  val parser = new scopt.OptionParser[Config]("bob") {
    opt[String]("check").action((x, c) => c.copy(check = x)).text("check - is a path to file")
    opt[Unit]("shell").action((_, c) => c.copy(shell = true)).text("shell - enter the shell")
    opt[Unit]("service").action((_, c) => c.copy(service = true)).text("service - run the http service")
  }

  parser.parse(args, Config()) match {
    case Some(Config(path, _, _)) if path.nonEmpty => checkCommand(path)
    case Some(Config(_, true, _)) => shellCommand()
    case Some(Config(_, _, true)) => serviceCommand()
    case None =>
  }
}
