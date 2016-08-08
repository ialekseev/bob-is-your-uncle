package com.ialekseev.bob.exec

import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob.{LexicalError, LexicalAnalysisFailed, BaseSpec}
import com.ialekseev.bob.analyzer.Analyzer
import org.mockito.Mockito
import scalaz._
import Scalaz._

class ExecutorSpec extends BaseSpec  {

  "Executor: checking" when {

    "analyzer returns errors" should {
      "return that errors" in {
        //arrange
        val anal = mock[Analyzer]
        val executor = new Executor {
          val scalaCompiler = null
          val analyzer = anal
        }
        Mockito.when(anal.analyze("source")).thenReturn(LexicalAnalysisFailed(Seq(LexicalError(10, 20), LexicalError(30, 40))).left)

        //act
        val result = executor.check("source")

        //assert
        result should be (LexicalAnalysisFailed(Seq(LexicalError(10, 20), LexicalError(30, 40))).left)
      }
    }

    "analyzer succeeds and returns scala-code" should {
      "compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Map("a" -> "1", "b" -> "2"), Webhook("abc/", HttpMethod.GET, Map.empty, Map.empty, none[Body]), ScalaCode("do()")).right
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned)
        Mockito.when(compiler.compile("""val a = "1"; val b = "2"""" + "\n" + "do()", 25)).thenReturn(classOf[BaseSpec].right)

        //act
        val result = executor.check("source")

        //assert
        result should be (resultToBeReturned)
      }
    }
  }
}
