package com.ialekseev.bob.exec

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import com.ialekseev.bob.{BaseSpec, Variable}
import com.ialekseev.bob.exec.Compiler._
import scalaz.Scalaz._
import akka.pattern.ask

class CompilerSpec extends TestKit(ActorSystem("compiler-specs")) with BaseSpec {

  "Compiling" when {

    "code has an error" should {
      "fail" in {
        //arrange
        val compilerActor = TestActorRef(new CompilerActor)

        //act
        val result = compilerActor ? CompilationRequest("vak = 3")

        //assert
        result.value.get.get.asInstanceOf[CompilationFailedResponse].r.head |> (a => (a.startOffset, a.pointOffset, a.endOffset)) should be (93,93,93)
      }
    }

    "code has several errors" should {
      "fail" in {
        //arrange
        val compilerActor = TestActorRef(new CompilerActor)

        //act
        val result = compilerActor ? CompilationRequest("a.call()", fields = "vak = 3;")

        //assert
        result.value.get.get.asInstanceOf[CompilationFailedResponse].r.length should be (2)
        (result.value.get.get.asInstanceOf[CompilationFailedResponse].r(0) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (73,73,73)
        (result.value.get.get.asInstanceOf[CompilationFailedResponse].r(1) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (101,101,101)
      }
    }

    "code is OK" should {
      "succeed" in {
        //arrange
        val compilerActor = TestActorRef(new CompilerActor)

        //act
        val result = compilerActor ? CompilationRequest("val k = 3")

        //assert
        result.value.get.get shouldBe a [CompilationSucceededResponse]
      }
    }
  }

  "Evaluating" when {

    "there are NO passed in variables" should {
      "succeed" in {
        //arrange
        val compilerActor = TestActorRef(new CompilerActor)
        val evaluatorActor = TestActorRef(new EvaluatorActor)
        val compiled = (compilerActor ? CompilationRequest("1 + 1")).value.get.get.asInstanceOf[CompilationSucceededResponse]

        //act
        val result = evaluatorActor ? EvaluationRequest(compiled.className, compiled.bytes, Nil)

        //assert
        result.value.get.get should be (EvaluationResponse(2))
      }
    }

    "there ARE passed in variables" should {
      "succeed" in {
        //arrange
        val compilerActor = TestActorRef(new CompilerActor)
        val evaluatorActor = TestActorRef(new EvaluatorActor)
        val compiled = (compilerActor ? CompilationRequest("a + b", fields = """var a = ""; var b = """"")).value.get.get.asInstanceOf[CompilationSucceededResponse]

        //act
        val result = evaluatorActor ? EvaluationRequest(compiled.className, compiled.bytes, List(Variable("a", "1"), Variable("b", "hi")))

        //assert
        result.value.get.get should be (EvaluationResponse("1hi"))
      }
    }
  }
}
