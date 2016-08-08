package com.ialekseev.bob.exec

import com.ialekseev.bob.StageFailed
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{AnalysisResult, ScalaCode}
import scalaz.Scalaz._
import scalaz._

trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler

  //todo: calculate ErrorCoordinates in case of an error and return CheckingFailed
  def check(source: String): StageFailed \/ AnalysisResult = {
    require(!source.isEmpty)

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

object Executor {
  case class CheckingFailed(analysisFailed: StageFailed, coordinates: ErrorCoordinates)
  case class ErrorCoordinates(line: Int, column: Int)
}
