package com.ialekseev.bob.exec

import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob._
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
        val result = executor.build("source")

        //assert
        result should be (LexicalAnalysisFailed(Seq(LexicalError(10, 20), LexicalError(30, 40))).left)
      }
    }

    "analyzer succeeds & returns constants + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Map("a" -> "1", "b" -> "2"), Webhook(HttpRequest("abc/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")).right
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned)
        Mockito.when(compiler.compile("do()", """var a = "1"; var b = "2"""")).thenReturn("abc".right)

        //act
        val result = executor.build("source")

        //assert
        result should be (resultToBeReturned)
      }
    }

    "analyzer succeeds & returns constant + bound variable in uri + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Map("a" -> "1"), Webhook(HttpRequest("abc/{$b}/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")).right
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned)
        Mockito.when(compiler.compile("do()", """var a = "1"; var b = """"")).thenReturn("abc".right)

        //act
        val result = executor.build("source")

        //assert
        result should be (resultToBeReturned)
      }
    }
  }
}
