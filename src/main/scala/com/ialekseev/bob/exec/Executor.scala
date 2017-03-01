package com.ialekseev.bob.exec

import com.ialekseev.bob.exec.analyzer.Analyzer
import com.ialekseev.bob.exec.analyzer.Analyzer.{AnalysisResult, ScalaCode, Webhook}
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.exec.Compiler._
import com.ialekseev.bob._
import org.json4s.JsonAST.JValue
import scala.util.Try
import scala.util.matching.Regex
import scalaz.Scalaz._
import akka.actor.ActorRef
import scalaz._
import scalaz.effect.IO
import scalaz.concurrent.Task
import scala.concurrent.Future
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import delorean._

trait Executor {
  val analyzer: Analyzer
  val compilerActor: ActorRef //todo: where to recover?
  val evaluatorActor: ActorRef

  private val variableRegexPattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r
  implicit val timeout = Timeout(5 seconds) //todo: move

  def build(source: String, externalVariables: List[(String, String)] = List.empty): Task[BuildFailed \/ Build] = {
    require(source.nonEmpty)

    def extractBoundVariablesFromStr(str: String): List[(String, String)] = {
      variableRegexPattern.findAllIn(str).matchData.map(m => (m.group(1), "")).toList
    }

    def extractBoundVariablesFromMap(map: Map[String, String]): List[(String, String)] = {
      map.map(m => extractBoundVariablesFromStr(m._2)).toList.flatten
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
      case \/-(result@ AnalysisResult(namespace, description, constants,  Webhook(HttpRequest(uri, _, headers, queryString, body)), ScalaCode(scalaCode))) => {
        val scalaImport = {
          "import com.ialekseev.bob.dsl._" ensuring {
             com.ialekseev.bob.dsl.Namespace != null
          }
        }

        val scalaVariables = {
          val localVariables = constants.toList |+| uri.map(extractBoundVariablesFromStr(_)).getOrElse(List.empty) |+|
            extractBoundVariablesFromMap(headers) |+| extractBoundVariablesFromMap(queryString) |+| extractBoundVariablesFromBody(body)

          val externalVariablesWithoutOvershadowedOnes = externalVariables.filter(v => !localVariables.exists(_._1 == v._1))

          val variables = externalVariablesWithoutOvershadowedOnes |+| localVariables

          """var request: HttpRequest = null; """ + variables.map(c => s"""var ${c._1} = "${c._2}"""").mkString("; ") ensuring {
            com.ialekseev.bob.dsl.HttpRequest != null
          }
        }

        val scalaImplicits = {
          s"""implicit val namespace = Namespace("${namespace.path}", "${namespace.name}"); """ +
          s"""implicit val description = Description("$description")""" ensuring {
             com.ialekseev.bob.dsl.Namespace("???", "???") != null && com.ialekseev.bob.dsl.Description("???") != null
          }
        }

        def amend(pos: Int) = {
          val compilerPositionAmendment = 93
          val start = source.indexOf("<scala>") + 7
          start + pos - compilerPositionAmendment - scalaVariables.length - scalaImport.length - scalaImplicits.length
        }

        (compilerActor ? CompilationRequest(scalaCode, scalaImport, scalaVariables, scalaImplicits)).toTask.map {
          case CompilationSucceededResponse(className, bytes) => Build(result, className, bytes).right
          case CompilationFailedResponse(errors) => CompilationFailed(errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset)))).left
        }
      }
      case analysisIssues@ -\/(_) => Task.now(analysisIssues)
    }
  }

  def run(incoming: HttpRequest, builds: List[Build]): Task[RunResult] = {

    def matchStr(buildStr: String, incomingStr: String): Option[List[(String, String)]] = {
      val patternStr = """^\Q""" + variableRegexPattern.replaceAllIn(buildStr, """\\E(?<$1>.+)\\Q""") + """\E$"""
      val groupNames = variableRegexPattern.findAllIn(buildStr).matchData.map(m => m.group(1)).toList
      val pattern = new Regex(patternStr, groupNames: _*)
      pattern.findFirstMatchIn(incomingStr).map(r => r.groupNames.map(n => (n, r.group(n))).toList)
    }

    def matchMap(buildMap: Map[String, String], incomingMap: Map[String, String]): Option[List[(String, String)]] = {
      if (buildMap.size > incomingMap.size) none
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
        case _ => none
      }
    }

    val matchedBuilds: List[(Build, List[(String, AnyRef)])] = builds.toList.map(build => {
      (some(List(("request", com.ialekseev.bob.dsl.HttpRequest(incoming.uri.getOrElse(""), incoming.method.toString, incoming.headers, incoming.queryString, incoming.body).asInstanceOf[AnyRef]))) |@|
       matchStr((s"${build.analysisResult.namespace.path}/${build.analysisResult.namespace.name}/${build.analysisResult.webhook.req.uri.map(_.trimSlashes).getOrElse("")}").trimSlashes.toLowerCase, incoming.uri.map(_.trimSlashes.toLowerCase).getOrElse("")) |@|
       matchMap(build.analysisResult.webhook.req.headers, incoming.headers) |@|
       matchMap(build.analysisResult.webhook.req.queryString, incoming.queryString) |@|
       matchBody(build.analysisResult.webhook.req.body, incoming.body) )(_ |+| _ |+| _ |+| _ |+| _).
       map(variables => (build, variables))
    }).flatten

    matchedBuilds.map(b => {
      (evaluatorActor ? EvaluationRequest(b._1.className, b._1.bytes, b._2)).mapTo[EvaluationResponse].toTask.map(r => SuccessfulRun(b._1, r.result)).handle {
        case _ => FailedRun(b._1)
      }
    }).sequenceU.map(RunResult(_))
  }
}

object Executor {
  case class Build(analysisResult: AnalysisResult, className: String, bytes: List[Byte])

  sealed trait Run
  case class SuccessfulRun(build: Build, result: Any) extends Run
  case class FailedRun(build: Build) extends Run
  case class RunResult(runs: List[Run])
}