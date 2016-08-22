package com.ialekseev.bob.exec

import com.ialekseev.bob.{HttpRequest, CompilationFailed, StageFailed}
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{Webhook, AnalysisResult, ScalaCode}
import scalaz.Scalaz._
import scalaz._

trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler

  def build(source: String): StageFailed \/ AnalysisResult = {
    def extractBoundVariables(str: String): Map[String, String] = {
      val pattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r
      pattern.findAllIn(str).matchData.map(m => (m.group(1), "")).toMap
    }

    analyzer.analyze(source) match {
      case \/-(result@ AnalysisResult(_, _, constants,  Webhook(HttpRequest(uri, _, _, _, _)), ScalaCode(scalaCode))) => {
        val variables = constants ++ extractBoundVariables(uri)
        val scalaVariables = variables.map(c => s"""var ${c._1} = "${c._2}"""").mkString("; ")

        def amend(pos: Int) = {
          val compilerPositionAmendment = 90
          val start = source.indexOf("<scala>") + 7
          start + pos - compilerPositionAmendment - scalaVariables.length
        }
        scalaCompiler.compile(scalaCode, scalaVariables).leftMap(f => CompilationFailed(f.errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset))))) >| result
      }
      case failed@ -\/(_) => failed
    }
  }
}