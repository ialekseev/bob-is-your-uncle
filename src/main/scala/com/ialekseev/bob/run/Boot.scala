package com.ialekseev.bob.run

import com.ialekseev.bob.exec.ScalaCompiler
import com.ialekseev.bob.run.commands.{Check, Service, Shell}

object Boot extends App with Command with Check with Shell with Service {
  def compiler = new ScalaCompiler

  (parser.parse(args, Config()) match {
    case Some(Config(true, _, _, _, _, _)) => shellCommand()
    case Some(Config(_, true, _, _, _, Arguments(Some(path)))) if path.nonEmpty => checkCommand(path)
    case Some(Config(_, _ , true, _, _, Arguments(path))) => serviceCommand(path.toList)
    case _ => showHelp()
  }).unsafePerformIO()
}

