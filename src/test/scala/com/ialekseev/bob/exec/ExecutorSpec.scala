package com.ialekseev.bob.exec

import com.ialekseev.bob._
import com.ialekseev.bob.analyzer.Analyzer
import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob.exec.Executor._
import org.json4s.JsonAST.{JBool, JInt, JObject, JString}
import org.mockito.Mockito
import scalaz.Scalaz._

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

    "analyzer succeeds & returns constants + scala-code with errors" should {
      "return error with amended offsets" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("<scala>do()<end>")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "2"""", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn(CompilationFailed(Seq(CompilationError(400,400,400, "bad!"))).left)

        //act
        val result = executor.build("<scala>do()<end>").unsafePerformSync

        //assert
        result should be  (CompilationFailed(Seq(CompilationError(128,128,128, "bad!"))).left)
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
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "2"""", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "there are external variables, analyzer succeeds & returns constants + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var ext1 = "1"; var ext2 = "str2"; var a = "1"; var b = "2"""", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source", List("ext1" -> "1", "ext2" -> "str2")).unsafePerformSync

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
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest(some("example/{$c}/{$d}"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "hi"; var c = ""; var d = """"", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "analyzer succeeds & returns constants + bound variables in uri, header, queryString + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest(some("example/{$c}/{$d}"), HttpMethod.GET, Map("header1" -> "{$h}", "header2" -> "head_{$header2}"), Map("query1" -> "{$q}_queryString", "query2" -> "{$query2}"), none[Body])), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "hi"; var c = ""; var d = ""; var h = ""; var header2 = ""; var q = ""; var query2 = """"", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "analyzer succeeds & returns constants + bound variables in body (string) + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest(some("example/1"), HttpMethod.GET, Map.empty, Map.empty, some(StringLiteralBody("hello {$name}! You are {$s}, aren't you?")))), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "hi"; var name = ""; var s = """"", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "analyzer succeeds & returns constants + bound variables in body (dictionary) + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest(some("example/1"), HttpMethod.GET, Map.empty, Map.empty, some(DictionaryBody(Map("hello"-> "{$name}!", "state" -> "{$s}, aren't you?"))))), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "hi"; var name = ""; var s = """"", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }

    "analyzer succeeds & returns constants + bound variables in body (json) + scala-code" should {
      "add variables & compile code with Scala compiler" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val resultToBeReturned = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "hi"), Webhook(HttpRequest(some("example/1"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("hello_{$name}_!"-> JString("my {$s} friend")))))), ScalaCode("do()"))
        Mockito.when(anal.analyze("source")).thenReturn(resultToBeReturned.right)
        Mockito.when(compiler.compile("do()", "import com.ialekseev.bob.dsl._", """var request: HttpRequest = null; var a = "1"; var b = "hi"; var name = ""; var s = """"", """implicit val namespace = Namespace("com", "create"); implicit val description = Description("cool")""")).thenReturn("abc".right)

        //act
        val result = executor.build("source").unsafePerformSync

        //assert
        result should be (Build(resultToBeReturned, "abc").right)
      }
    }
  }

  "Executor: running" when {

    import com.ialekseev.bob.dsl.{HttpRequest => HttpRequestEx}

    "there are no matching builds" should {
      "return empty" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("com/create/example1/1/2/"), HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example2/{$a}/2/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")),"super"))

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq.empty))
      }
    }

    "there are no matching builds (incoming POST request doesn't have 'body')" should {
      "return empty" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.POST, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.POST, Map.empty, Map.empty, some(StringLiteralBody("hello {$name}! You are {$s}, aren't you?")))), ScalaCode("do()")), "abc"))

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq.empty))
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
        val incoming = HttpRequest(some("com/create/"), HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(none, HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/", "GET", Map.empty, Map.empty, none)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) - even though letter case is different)" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("COM/create/EXAMPLE/"), HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("COM/create/EXAMPLE/", "GET", Map.empty, Map.empty, none)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) - even though slash edges are different)" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("/com/create/EXAMPLE/"), HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("/example"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("/com/create/EXAMPLE/", "GET", Map.empty, Map.empty, none)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
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
        val incoming = HttpRequest(some("com/create"), HttpMethod.GET, Map("header1" -> "secret", "header2" -> "xxx", "header3" -> "yyy"), Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(none, HttpMethod.GET, Map("header1" -> "secret", "header2" -> "xxx"), Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create", "GET", Map("header1" -> "secret", "header2" -> "xxx", "header3" -> "yyy"), Map.empty, none)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'headers' (with vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("com/create"), HttpMethod.GET, Map("header1" -> "secret", "header2" -> "app_xxx"), Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(none, HttpMethod.GET, Map("header1" -> "{$h1}", "header2" -> "app_{$h2}"), Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create", "GET", Map("header1" -> "secret", "header2" -> "app_xxx"), Map.empty, none), "h1" -> "secret", "h2" -> "xxx"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
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
        val incoming = HttpRequest(some("com/create/example/1/2/"), HttpMethod.GET, Map.empty, Map.empty, none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/{$a}/{$b}/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/1/2/", "GET", Map.empty, Map.empty, none), "a" -> "1", "b" -> "2"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'queryString' (with vars)) - CASE INSENSITIVE KEYS" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.POST, Map.empty, Map("q1" -> "start_john_end", "Q2" -> "smith", "Q3" -> "super"), none)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.POST, Map.empty, Map("Q1" -> "start_{$name}_end", "q2" -> "{$surname}", "Q3" -> "super"), none[Body])), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "POST", Map.empty, Map("q1" -> "start_john_end", "Q2" -> "smith", "Q3" -> "super"), none), "name" -> "john", "surname" -> "smith"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (with vars) and 'body (string)' (with vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(StringLiteralBody("hello John! You are fine, aren't you?"))
        val incoming = HttpRequest(some("com/create/example/1/2/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/{$a}/{$b}/"), HttpMethod.GET, Map.empty, Map.empty, some(StringLiteralBody("hello {$name}! You are {$s}, aren't you?")))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/1/2/", "GET", Map.empty, Map.empty, body), "a" -> "1", "b" -> "2", "name" -> "John", "s" -> "fine"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'body (dictionary)' (with vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(DictionaryBody(Map("hello"-> "John", "state" -> "fine, aren't you?")))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(DictionaryBody(Map("hello"-> "{$name}", "state" -> "{$s}, aren't you?"))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body), "name" -> "John", "s" -> "fine"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'body (json)' (without vars))" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(JsonBody(JObject("a"-> JString("1"), "b" -> JInt(1))))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("b" -> JInt(1), "a"-> JString("1")))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'body (json)' (without vars)) for incoming request with excessive body data" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(JsonBody(JObject("a"-> JObject("a1" -> JString("1"), "a2" -> JInt(2)), "c" -> JString("2"), "b" -> JInt(1))))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("b" -> JInt(1), "a"-> JObject("a2" -> JInt(2))))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body)))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'body (json)' (with vars)) for incoming request with excessive body data" should {
      "run it" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(JsonBody(JObject("a"-> JObject("a1" -> JString("1"), "a2" -> JString("before_hello_after")), "c" -> JString("2"), "b" -> JString("1"))))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("b" -> JString("{$b}"), "a"-> JObject("a2" -> JString("before_{$hel}_after"))))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body), "hel" -> "hello", "b" -> "1"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a matching build (with matching 'uri' (without vars) and 'body (json)' (with standalone vars)) for incoming request with Int data for the matching vars" should {
      "run it with all variables as Strings" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(JsonBody(JObject("a"-> JObject("a1" -> JString("1"), "a2" -> JInt(2)), "c" -> JString("2"), "b" -> JBool(true))))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("b" -> JString("{$b}"), "a"-> JObject("a2" -> JString("{$hel}"))))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body), "hel" -> "2", "b" -> "true"))).thenReturn("1")

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result should be (RunResult(Seq(SuccessfulRun(builds(0), "1"))))
      }
    }

    "there is a failed run (compiler throws an exception)" should {
      "return a failure for that run" in {
        //arrange
        val anal = mock[Analyzer]
        val compiler = mock[ScalaCompiler]
        val executor = new Executor {
          val scalaCompiler = compiler
          val analyzer = anal
        }
        val body = some(JsonBody(JObject("a"-> JObject("a1" -> JString("1"), "a2" -> JInt(2)), "c" -> JString("2"), "b" -> JBool(true))))
        val incoming = HttpRequest(some("com/create/example/"), HttpMethod.GET, Map.empty, Map.empty, body)
        val builds = Seq(Build(AnalysisResult(Namespace("com", "create"), "cool", Seq.empty, Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, some(JsonBody(JObject("b" -> JString("{$b}"), "a"-> JObject("a2" -> JString("{$hel}"))))))), ScalaCode("do()")), "abc"))
        Mockito.when(compiler.eval("abc", Seq("request" -> HttpRequestEx("com/create/example/", "GET", Map.empty, Map.empty, body), "hel" -> "2", "b" -> "true"))).thenThrow(new NullPointerException("bang!"))

        //act
        val result = executor.run(incoming, builds).unsafePerformSync

        //assert
        result.runs(0).asInstanceOf[FailedRun].error shouldBe a [NullPointerException]
        result.runs(0).asInstanceOf[FailedRun].error.getMessage should be ("bang!")
      }
    }
  }
}
