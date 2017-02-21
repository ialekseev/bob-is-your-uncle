package com.ialekseev.bob.run.boot

import akka.actor.{Props, ActorSystem}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{Executor, EvaluatorActor, CompilerActor}
import com.ialekseev.bob.run._
import com.ialekseev.bob.run.cli.{Check, BaseCommand, Service, Shell}
import com.ialekseev.bob.run.http.BaseHttpService

object Boot extends App with BaseCommand with BaseHttpService with HttpServiceUnsafe with Check with Shell with Service {
  val system = ActorSystem("bob's system")

  val exec = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor])
    val evaluatorActor = system.actorOf(Props[EvaluatorActor])
  }

  (parser.parse(args, Config()) match {
    case Some(Config(true, _, _, _, _, _)) => shellCommand()
    case Some(Config(_, true, _, _, _, Arguments(Some(path)))) if path.nonEmpty => checkCommand(path)
    case Some(Config(_, _ , true, _, _, Arguments(path))) => serviceCommand(path.toList)
    case _ => showHelp().toTask
  }).unsafePerformSync
}

