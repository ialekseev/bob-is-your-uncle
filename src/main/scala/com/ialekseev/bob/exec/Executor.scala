package com.ialekseev.bob.exec

import com.ialekseev.bob.{CompilationFailed, StageFailed}
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{AnalysisResult, ScalaCode}
import scalaz.Scalaz._
import scalaz._

trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler

  def check(source: String): StageFailed \/ AnalysisResult = {
    analyzer.analyze(source) match {
      case \/-(result@ AnalysisResult(_, _, constants, _, ScalaCode(code))) => {
        val scalaConstants = constants.map(c => s"""val ${c._1} = "${c._2}"""").mkString("", "; ", "\n")
        val scalaCode = scalaConstants + code
        val start = source.indexOf("<scala>") + 7
        def amend(pos: Int) = start + pos - scalaConstants.length
        scalaCompiler.compile(scalaCode).leftMap(f => CompilationFailed(f.errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset))))) >| result
      }
      case failed@ -\/(_) => failed
    }
  }
}