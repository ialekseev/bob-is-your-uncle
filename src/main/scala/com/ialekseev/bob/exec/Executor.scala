package com.ialekseev.bob.exec

import com.ialekseev.bob.exec.Executor.{Run, BuildFailed, Build}
import com.ialekseev.bob.{HttpRequest, CompilationFailed, StageFailed}
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{Webhook, AnalysisResult, ScalaCode}
import scala.util.matching.Regex
import scalaz._
import scalaz.concurrent.Task

//todo: refactor the regex mess
trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler

  def build(source: String): Task[BuildFailed \/ Build] = {
    def extractBoundVariables(str: String): Seq[(String, String)] = {
      val pattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r
      pattern.findAllIn(str).matchData.map(m => (m.group(1), "")).toSeq
    }

    analyzer.analyze(source) match {
      case \/-(result@ AnalysisResult(_, _, constants,  Webhook(HttpRequest(uri, _, _, _, _)), ScalaCode(scalaCode))) => {
        val variables = constants ++ extractBoundVariables(uri) //todo: just uri for now
        val scalaVariables = variables.map(c => s"""var ${c._1} = "${c._2}"""").mkString("; ")

        def amend(pos: Int) = {
          val compilerPositionAmendment = 90
          val start = source.indexOf("<scala>") + 7
          start + pos - compilerPositionAmendment - scalaVariables.length
        }
        Task {
          scalaCompiler.compile(scalaCode, scalaVariables).
            leftMap(f => CompilationFailed(f.errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset))))).
            map(Build(result, _))
        }
      }
      case failed@ -\/(_) => Task.now(failed)
    }
  }

  //todo: test heavily
  def run(incoming: HttpRequest, builds: Seq[Build]): Task[Seq[Run]] = {

    def matchStr(incomingStr: String): Option[Seq[(String, String)]] = {
       val patternStr = ("^" + """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r.replaceAllIn(incomingStr, """(?<$1>.+)""") + "$")
       val groupNames = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r.findAllIn(incomingStr).matchData.map(m => m.group(1)).toSeq
       val pattern = new Regex(patternStr, groupNames: _*)
       pattern.findFirstMatchIn(incoming.uri).map(r => {
         r.groupNames.map(n => (n, r.group(n)))
       })
    }

    val matchedBuilds = builds.map(build => {
      matchStr(build.analysisResult.webhook.req.uri).map(variables => (build, variables)) //todo: just uri for now
    }).flatten

    Task {
      matchedBuilds.map(b => {
        Run(b._1, scalaCompiler.eval[Any](b._1.codeFileName, b._2))
      })
    }
  }
}

object Executor {
  type BuildFailed = StageFailed
  case class Build(analysisResult: AnalysisResult, codeFileName: String)

  case class Run(build: Build, result: Any)
}