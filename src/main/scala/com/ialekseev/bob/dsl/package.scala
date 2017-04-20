package com.ialekseev.bob

import org.json4s.JObject
import org.json4s.JsonAST.JValue
import scalaz._
import scalaz.Scalaz._
import org.json4s._
import org.json4s.native.JsonMethods._

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

/*********************************************************************************/
  implicit val httpRequest: HttpRequest = null

  implicit val jValueAct: Show[JValue] = Show.shows(v => pretty(render(v)))
  implicit val jObjectAct: Show[JObject] = Show.shows(v => pretty(render(v)))

  case class NextAction[A, B](desc: String, act: A => B)
  case class ActionLog(desc: String, result: String)

  case class ActionResult[A](result: A, logs: Vector[ActionLog]) {
    def ~[B](next: NextAction[A, B])(implicit show: Show[B] = Show.showFromToString[B]): ActionResult[B] = {
      val nextResult = next.act(result)
      ActionResult(nextResult, logs :+ ActionLog(next.desc, show.shows(nextResult)))
    }
  }

  def action[T](desc: String)(act: HttpRequest => T)(implicit show: Show[T] = Show.showFromToString[T]): ActionResult[T] = {
    val httpRequest = implicitly[HttpRequest]
    val result = act(httpRequest)
    ActionResult(result, Vector(ActionLog(desc, show.shows(result))))
  }

  def next[A, B](desc: String)(act: A => B): NextAction[A, B] = NextAction(desc, act)
  /*********************************************************************************/

  implicit class ScalajHttpRequestWrapper(httpRequest: scalaj.http.HttpRequest){
    def json: JValue = parse(httpRequest.asString.body)
    def text: String = httpRequest.asString.body
  }
}


