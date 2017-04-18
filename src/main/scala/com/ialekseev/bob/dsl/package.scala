package com.ialekseev.bob

import com.ialekseev.bob._
import org.json4s.JObject
import org.json4s.JsonAST.JValue
import scalaz._
import scalaz.Scalaz._

package object dsl {
  case class Namespace(path: String, name: String)
  case class Description(text: String)
  case class HttpRequest(uri: String, method: String, headers: Map[String, String], queryString: Map[String, String], body: Option[Body]) {
    val bodyStr: Option[String] = body match {
      case Some(StringLiteralBody(str)) => some(str)
      case _ => none
    }

    val bodyDic: Option[Map[String, String]] = body match {
      case Some(DictionaryBody(dic)) => some(dic)
      case _ => none
    }

    val bodyJson: Option[JValue] = body match {
      case Some(JsonBody(json)) => some(json)
      case _ => none
    }
  }

  object console {
    def show(obj: Any)(implicit namespace: Namespace) = {
      println(Console.CYAN + s"${namespace.path}#${namespace.name}:" + Console.RESET + " " + obj)
    }

    def showMe(implicit namespace: Namespace, description: Description) = {
      println(Console.CYAN + s"${namespace.path}#${namespace.name}:" + Console.RESET + " " + description.text)
    }
  }

  //todo: Currently pipeline is of one type(eg, JObject) - try to use shapeless' HList to allow steps of different types. Like that: each step returns HList with newly added value. At the end we fold it to get a result

  implicit val httpRequest: HttpRequest = null //hack, will be provided in CompilerActor

  case class PipelineResult[T](value: T, logs: Vector[String])
  case class FirstAction[T](description: String, act: HttpRequest => T)
  case class Action[T](description: String, act: T => T)
  object run

  case class Pipeline[T](first: FirstAction[T], next: Vector[Action[T]]) {
    def ~(nextAction: Action[T]): Pipeline[T] = this.copy(next = next :+ nextAction)

    def ~(runAction: run.type): PipelineResult[T] = {
      val httpRequest = implicitly[HttpRequest]
      val afterFirstAction = first.act(httpRequest)

      val result = next.foldLeft((afterFirstAction, Vector(first.description)))((acc, action) => {
        (action.act(acc._1), acc._2 :+ action.description)
      })

      PipelineResult(result._1, result._2)
    }
  }

  def action[T](description: String)(act: HttpRequest => T): Pipeline[T] = Pipeline(FirstAction(description, act), Vector.empty)
  def next[T](description: String)(act: T => T): Action[T] = Action(description, act)
}
