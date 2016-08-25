package com.ialekseev.bob.exec

import com.ialekseev.bob.exec.Executor.{Run, BuildFailed, Build}
import com.ialekseev.bob.{HttpRequest, CompilationFailed, StageFailed}
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{Webhook, AnalysisResult, ScalaCode}
import scala.util.matching.Regex
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler
  private val variableRegexPattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r

  def build(source: String): Task[BuildFailed \/ Build] = {
    def extractBoundVariables(str: String): Seq[(String, String)] = {
      variableRegexPattern.findAllIn(str).matchData.map(m => (m.group(1), "")).toSeq
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

  def run(incoming: HttpRequest, builds: Seq[Build]): Task[Seq[Run]] = {

    def matchStr(buildStr: String, incomingStr: String): Option[List[(String, String)]] = {
       val patternStr = ("^" + variableRegexPattern.replaceAllIn(buildStr, """(?<$1>.+)""") + "$")
       val groupNames = variableRegexPattern.findAllIn(buildStr).matchData.map(m => m.group(1)).toSeq
       val pattern = new Regex(patternStr, groupNames: _*)
       pattern.findFirstMatchIn(incomingStr).map(r => {
         r.groupNames.map(n => (n, r.group(n))).toList
       })
    }

    //todo: key comparison should be case insensitive
    def matchMap(buildMap: Map[String, String], incomingMap: Map[String, String]): Option[List[(String, String)]] = {
      if (buildMap.size != incomingMap.size) none
      else buildMap.toList.map(b => (incomingMap.get(b._1) >>= (in => matchStr(b._2, in)))).sequence.map(v => v.suml)
    }

    val matchedBuilds = builds.map(build => { //todo: just uri and headers for now
      (matchStr(build.analysisResult.webhook.req.uri, incoming.uri) |@|
        matchMap(build.analysisResult.webhook.req.headers, incoming.headers))(_ |+| _).
        map(variables => (build, variables))
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