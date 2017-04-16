package com.ialekseev.bob.run.boot

import java.nio.file.Paths
import akka.actor.{ActorSystem, Props}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{CompilerActor, EvaluatorActor, Executor}
import com.ialekseev.bob.run._
import com.ialekseev.bob.run.cli._
import com.ialekseev.bob.run.TaskConversions._
import com.ialekseev.bob.run.http.BaseHttpService

object Boot extends App with BaseCommand with BaseHttpService with Check with Shell with Service {
  val system = ActorSystem("bob-system")
  implicit val executionContext = system.dispatcher

  val sandboxPathPrefix = "sandbox"
  val hookPathPrefix = "hook"
  val buildStateActor = system.actorOf(Props[BuildStateActor])
  val sourceStateActor = system.actorOf(Props(new SourceStateActor()))

  val exec = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor].withDispatcher("compiler-blocking-dispatcher"))
    val evaluatorActor = system.actorOf(Props[EvaluatorActor].withDispatcher("evaluator-blocking-dispatcher"))
  }

  (parser.parse(args, Config()) match {
    case Some(Config(true,_,_,_,_,_)) => shellCommand()
    case Some(Config(_,true,_,_,_,Arguments(Some(path)))) if path.nonEmpty => checkCommand(Paths.get(path))
    case Some(Config(_,_,true,_,_,Arguments(path))) => serviceCommand(path.map(Paths.get(_)).toList)
    case _ => showHelp().toTask
  }).unsafePerformSync

  system.terminate()
}

