package com.ialekseev.bob.analyzer

import org.json4s.JsonAST.JValue
import scalaz._

trait Analyzer {
  def analyze(source: String): AnalysisError \/ AnalysisResult
}

class AnalyzerImpl extends Analyzer {
  def analyze(source: String): AnalysisError \/ AnalysisResult = {
    //todo
    ???
  }
}

case class AnalysisResult(namespace: Namespace, description: String, constants: Map[String, String], webhook: Webhook, process: Code)

case class Namespace(path: String, name: String)
case class Webhook(uri: String, method: Option[String], headers: Map[String, String], queryString: Map[String, String], body: Option[Body])

sealed trait Body
case class StringLiteralBody(text: String) extends Body
case class DictionaryBody(dic: Map[String, String]) extends Body
case class JsonBody(j: JValue) extends Body

sealed trait Code
case class ScalaCode(code: String) extends Code