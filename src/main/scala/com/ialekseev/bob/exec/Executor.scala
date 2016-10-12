package com.ialekseev.bob.exec

import com.ialekseev.bob.exec.Executor.{Run, BuildFailed, Build}
import com.ialekseev.bob.{HttpRequest, CompilationFailed, StageFailed, Body, StringLiteralBody, DictionaryBody, JsonBody}
import com.ialekseev.bob.analyzer.{Analyzer}
import com.ialekseev.bob.analyzer.Analyzer.{Webhook, AnalysisResult, ScalaCode}
import org.json4s.JsonAST.JValue
import scala.util.matching.Regex
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

trait Executor {
  val analyzer: Analyzer
  val scalaCompiler: ScalaCompiler
  private val variableRegexPattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r

  def build(source: String): Task[BuildFailed \/ Build] = {
    def extractBoundVariablesFromStr(str: String): List[(String, String)] = {
      variableRegexPattern.findAllIn(str).matchData.map(m => (m.group(1), "")).toList
    }

    def extractBoundVariablesFromMap(map: Map[String, String]): List[(String, String)] = {
      map.map(m => extractBoundVariablesFromStr(m._2)).toList.map(v => v.suml)
    }

    def extractBoundVariablesFromBody(body: Option[Body]): List[(String, String)] = {
      body match {
        case Some(StringLiteralBody(str)) => extractBoundVariablesFromStr(str)
        case Some(DictionaryBody(dic)) => extractBoundVariablesFromMap(dic)
        case Some(JsonBody(json)) => extractBoundVariablesFromStr(json.toString)
        case None => List.empty
        case b => sys.error(s"The body ($b) is not supported!")
      }
    }

    analyzer.analyze(source) match {
      case \/-(result@ AnalysisResult(_, _, constants,  Webhook(HttpRequest(uri, _, headers, queryString, body)), ScalaCode(scalaCode))) => {
        val variables = constants.toList |+| extractBoundVariablesFromStr(uri) |+|
          extractBoundVariablesFromMap(headers) |+| extractBoundVariablesFromMap(queryString) |+| extractBoundVariablesFromBody(body)
        val scalaVariables = variables.map(c => s"""var ${c._1} = "${c._2}"""").mkString("; ")

        def amend(pos: Int) = {
          val compilerPositionAmendment = 90
          val start = source.indexOf("<scala>") + 7
          start + pos - compilerPositionAmendment - scalaVariables.length - 1
        }

        Task {
          scalaCompiler.compile(scalaCode, scalaVariables).
            leftMap(f => CompilationFailed(f.errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset))))).
            map(Build(result, _))
        }
      }
      case failed@ -\/(_) => Task.now(failed)
    }
  }

  def run(incoming: HttpRequest, builds: Seq[Build]): Task[Seq[Run]] = {

    def matchStr(buildStr: String, incomingStr: String): Option[List[(String, String)]] = {
      val patternStr = """^\Q""" + variableRegexPattern.replaceAllIn(buildStr, """\\E(?<$1>.+)\\Q""") + """\E$"""
      val groupNames = variableRegexPattern.findAllIn(buildStr).matchData.map(m => m.group(1)).toSeq
      val pattern = new Regex(patternStr, groupNames: _*)
      pattern.findFirstMatchIn(incomingStr).map(r => r.groupNames.map(n => (n, r.group(n))).toList)
    }

    def matchMap(buildMap: Map[String, String], incomingMap: Map[String, String]): Option[List[(String, String)]] = {
      if (buildMap.size != incomingMap.size) none
      else {
        val incomingMapWithUpperCaseKeys = incomingMap.mapKeys(_.toUpperCase)
        buildMap.toList.map(b => (incomingMapWithUpperCaseKeys.get(b._1.toUpperCase) >>= (in => matchStr(b._2, in)))).sequence.map(v => v.suml)
      }
    }

    def matchJson(buildJson: JValue, incomingMap: JValue): Option[List[(String, String)]] = {
      import org.json4s._
      import org.json4s.native.JsonMethods._
      incomingMap.diff(buildJson) match {
        case Diff(JNothing, JNothing, _) => some(List.empty)
        case Diff(changedBuild, JNothing, _) => {
          val changedIncoming = changedBuild.diff(incomingMap).changed
          val changedBuildStr = compact(render(changedBuild)).replaceAll("\"", "")
          val changedIncomingStr = compact(render(changedIncoming)).replaceAll("\"", "")
          matchStr(changedBuildStr, changedIncomingStr)
        }
        case _ => sys.error("not yet supported json match!")
      }
    }

    def matchBody(buildBody: Option[Body], incomingBody: Option[Body]): Option[List[(String, String)]] = {
      (buildBody, incomingBody) match {
        case (Some(StringLiteralBody(bStr)), Some(StringLiteralBody(iStr))) => matchStr(bStr, iStr)
        case (Some(DictionaryBody(bDic)), Some(DictionaryBody(iDic))) => matchMap(bDic, iDic)
        case (Some(JsonBody(bJson)), Some(JsonBody(iJson))) => matchJson(bJson, iJson)
        case (None, None) => some(List.empty)
        case m => sys.error(s"The match ($m) is not supported!")
      }
    }

    /*todo: should uri/path collation be case-insensitive?
            should we drop starting/ending slashes?
            should we support absolute URIs (currently we assume working with PATHs)?*/

    val matchedBuilds = builds.map(build => {
      (matchStr(build.analysisResult.webhook.req.uri, incoming.uri) |@|
       matchMap(build.analysisResult.webhook.req.headers, incoming.headers) |@|
       matchMap(build.analysisResult.webhook.req.queryString, incoming.queryString) |@|
       matchBody(build.analysisResult.webhook.req.body, incoming.body) )(_ |+| _ |+| _ |+| _).
       map(variables => (build, variables))
    }).flatten

    Task {
      matchedBuilds.map(b => {
        Run(b._1, scalaCompiler.eval[Any](b._1.codeFileName, b._2))
      })
    }
  }
}

object Executor {
  type BuildFailed = StageFailed
  case class Build(analysisResult: AnalysisResult, codeFileName: String)

  case class Run(build: Build, result: Any)
}