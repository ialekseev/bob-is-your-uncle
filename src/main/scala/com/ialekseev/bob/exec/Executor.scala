package com.ialekseev.bob.exec

import com.ialekseev.bob.StageFailed
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
        scalaCompiler.compile(scalaCode, scalaConstants.length) >| result
      }
      case failed@ -\/(_) => failed
    }
  }
}