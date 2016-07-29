package com.ialekseev.bob

import com.ialekseev.bob.analyzer.Analyzer
import com.ialekseev.bob.analyzer.Analyzer.{AnalysisResult, ScalaCode}
import com.ialekseev.bob.exec.ScalaCompiler
import scalaz._
import Scalaz._

trait Executor {
  val analyzer: Analyzer
  def check(source: String): AnalysisFailed \/ AnalysisResult
}

object Executor {
  def apply = new Executor {
    val analyzer = Analyzer()

    def check(source: String): AnalysisFailed \/ AnalysisResult = {
      require(!source.isEmpty)

      analyzer.analyze(source) match {
        case \/-(result@ AnalysisResult(_, _, constants, _, ScalaCode(code))) => {
          val scalaConstants = constants.map(c => s"""val ${c._1} = "${c._2}""").mkString(";") + "\n"
          val scalaCode = scalaConstants + code
          ScalaCompiler.compile(scalaCode) >| result
        }
        case failed@ -\/(_) => failed
      }
    }
  }
}

