package com.ialekseev.bob

import org.json4s.JsonAST.JValue
import scalaz._
import Scalaz._

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

    /*val bodyJson: Option[JValue] = body match { //todo: now not gonna work without import & adding lib to the classpath
      case Some(JsonBody(json)) => some(json)
      case _ => none
    }*/
  }

  object console {
    def show(obj: Any)(implicit namespace: Namespace) = {
      println(Console.CYAN + s"${namespace.path}#${namespace.name}:" + Console.RESET + " " + obj)
    }

    def showMe(implicit namespace: Namespace, description: Description) = {
      println(Console.CYAN + s"${namespace.path}#${namespace.name}:" + Console.RESET + " " + description.text)
    }
  }
}
