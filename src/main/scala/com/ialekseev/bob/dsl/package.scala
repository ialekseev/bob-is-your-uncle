package com.ialekseev.bob

package object dsl {
  case class Namespace(path: String, name: String)
  case class HttpRequest(uri: String, method: String, headers: Map[String, String], queryString: Map[String, String])

  def console(obj: Any)(implicit namespace: Namespace) = {
    println(Console.CYAN + s"${namespace.path}#${namespace.name}:" + Console.RESET + " " + obj)
  }
}
