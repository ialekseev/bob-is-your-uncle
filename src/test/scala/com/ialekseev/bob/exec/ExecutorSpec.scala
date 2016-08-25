package com.ialekseev.bob.exec

import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob._
import com.ialekseev.bob.analyzer.Analyzer
import com.ialekseev.bob.exec.Executor.{Run, Build}
import org.mockito.Mockito
import scalaz._
import Scalaz._

class ExecutorSpec extends BaseSpec  {

  "Executor: building" when {

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
        val result = executor.build("source").unsafePerformSync

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
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest("example/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", """var a = "1"; var b = "2"""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "analyzer succeeds & returns constants + bound variables in uri + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest("example/{$c}/{$d}", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", """var a = "1"; var b = "hi"; var c = ""; var d = """"")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }
  }

  "Executor: running" when {

    "there are no matching builds" should {
      "return empty" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest("example.com/1/2/", HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest("example.ru/{$a}/2/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")),"super"))

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (Seq.empty)
      }
    }

    "there is a matching build (with matching 'uri' (without vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest("example.com/", HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest("example.com/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq.empty)).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (Seq(Run(builds(0), "1")))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'headers' (without vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest("example.com/", HttpMethod.GET, Map("header1" -> "secret", "header2" -> "xxx"), Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest("example.com/", HttpMethod.GET, Map("header1" -> "secret", "header2" -> "xxx"), Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq.empty)).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (Seq(Run(builds(0), "1")))
      }
    }

    "there is a matching build (with matching 'uri' (with vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest("example.com/1/2/", HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest("example.com/{$a}/{$b}/", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("a" -> "1", "b" -> "2"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (Seq(Run(builds(0), "1")))
      }
    }
  }
}
