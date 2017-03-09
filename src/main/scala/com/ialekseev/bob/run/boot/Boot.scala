package com.ialekseev.bob.run.boot

import akka.actor.{Props, ActorSystem}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{Executor, EvaluatorActor, CompilerActor}
import com.ialekseev.bob.run._
import com.ialekseev.bob.run.cli._
import com.ialekseev.bob.run.http.BaseHttpService

object Boot extends App with BaseCommand with BaseHttpService with HttpServiceUnsafe with Check with Shell with Service with Sandbox {
  val system = ActorSystem("bob-system")

  val exec = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor].withDispatcher("compiler-blocking-dispatcher"))
    val evaluatorActor = system.actorOf(Props[EvaluatorActor].withDispatcher("evaluator-blocking-dispatcher"))
  }

  (parser.parse(args, Config()) match {
    case Some(Config(true,_,_,_,_,_,_)) => shellCommand()
    case Some(Config(_,true,_,_,_,_,Arguments(Some(path)))) if path.nonEmpty => checkCommand(path)
    case Some(Config(_,_,true,_,_,_,Arguments(path))) => serviceCommand(path.toList)
    case Some(Config(_,_,_,true,_,_,Arguments(path))) => sandboxCommand(path)
    case _ => showHelp().toTask
  }).unsafePerformSync
}

