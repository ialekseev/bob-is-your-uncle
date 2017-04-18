package com.ialekseev.bob.exec

import com.ialekseev.bob.exec.analyzer.{Analyzer, Token}
import com.ialekseev.bob.exec.analyzer.Analyzer.{AnalysisResult, ScalaCode, Webhook}
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.exec.Compiler._
import com.ialekseev.bob._
import com.ialekseev.bob.run.TaskConversions._
import org.json4s.JsonAST.JValue
import scala.util.matching.Regex
import scalaz.Scalaz._
import akka.actor.ActorRef
import scalaz._
import scalaz.concurrent.Task
import akka.util.Timeout
import scala.concurrent.ExecutionContext

abstract class Executor(implicit executionContext: ExecutionContext, timeout: Timeout) {
  val analyzer: Analyzer
  val compilerActor: ActorRef //todo: recover & log?
  val evaluatorActor: ActorRef

  private val variableRegexPattern = """\{\$([a-zA-Z]+[a-zA-Z0-9]*)\}""".r

  def build(source: String, externalVariables: List[Variable[String]] = List.empty): Task[BuildFailed \/ Build] = {
    require(source.nonEmpty)

    def extractBoundVariablesFromStr(str: String): List[Variable[String]] = {
      variableRegexPattern.findAllIn(str).matchData.map(m => Variable(m.group(1), "")).toList
    }

    def extractBoundVariablesFromMap(map: Map[String, String]): List[Variable[String]] = {
      map.map(m => extractBoundVariablesFromStr(m._2)).toList.flatten
    }

    def extractBoundVariablesFromBody(body: Option[Body]): List[Variable[String]] = {
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
          val localVariables = constants |+| uri.map(extractBoundVariablesFromStr(_)).getOrElse(List.empty) |+|
            extractBoundVariablesFromMap(headers) |+| extractBoundVariablesFromMap(queryString) |+| extractBoundVariablesFromBody(body)

          val externalVariablesWithoutOvershadowedOnes = externalVariables.filter(v => !localVariables.exists(_.name == v.name))

          val variables = externalVariablesWithoutOvershadowedOnes |+| localVariables

          """var request: HttpRequest = null; """ + variables.map(c => s"""var ${c.name} = "${c.value}"""").mkString("; ") ensuring {
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
          val start = source.indexOf(Token.Block.`@process`.beginWord) + Token.Block.`@process`.beginWord.length
          start + pos - compilerPositionAmendment - scalaVariables.length - scalaImport.length - scalaImplicits.length
        }

        compilerActor.tAsk[Any](CompilationRequest(scalaCode, scalaImport, scalaVariables, scalaImplicits)).map {
          case CompilationSucceededResponse(className, bytes) => Build(result, className, bytes).right
          case CompilationFailedResponse(errors) => CompilationFailed(errors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset)))).left
        }
      }
      case analysisIssues@ -\/(_) => Task.now(analysisIssues)
    }
  }

  def run(incoming: HttpRequest, builds: List[Build]): Task[RunResult] = {

    def matchStr(buildStr: String, incomingStr: String): Option[List[Variable[String]]] = {
      val patternStr = """^\Q""" + variableRegexPattern.replaceAllIn(buildStr, """\\E(?<$1>.+)\\Q""") + """\E$"""
      val groupNames = variableRegexPattern.findAllIn(buildStr).matchData.map(m => m.group(1)).toList
      val pattern = new Regex(patternStr, groupNames: _*)
      pattern.findFirstMatchIn(incomingStr).map(r => r.groupNames.map(n => Variable(n, r.group(n))).toList)
    }

    def matchMap(buildMap: Map[String, String], incomingMap: Map[String, String]): Option[List[Variable[String]]] = {
      if (buildMap.size > incomingMap.size) none
      else {
        val incomingMapWithUpperCaseKeys = incomingMap.mapKeys(_.toUpperCase)
        buildMap.toList.map(b => (incomingMapWithUpperCaseKeys.get(b._1.toUpperCase) >>= (in => matchStr(b._2, in)))).sequence.map(v => v.suml)
      }
    }

    def matchJson(buildJson: JValue, incomingMap: JValue): Option[List[Variable[String]]] = {
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

    def matchBody(buildBody: Option[Body], incomingBody: Option[Body]): Option[List[Variable[String]]] = {
      (buildBody, incomingBody) match {
        case (Some(StringLiteralBody(bStr)), Some(StringLiteralBody(iStr))) => matchStr(bStr, iStr)
        case (Some(DictionaryBody(bDic)), Some(DictionaryBody(iDic))) => matchMap(bDic, iDic)
        case (Some(JsonBody(bJson)), Some(JsonBody(iJson))) => matchJson(bJson, iJson)
        case (None, None) => some(List.empty)
        case _ => none
      }
    }

    val matchedBuilds: List[(Build, List[Variable[AnyRef]])] = builds.map(build => {
      (some(List(Variable("request", com.ialekseev.bob.dsl.HttpRequest(incoming.uri.getOrElse(""), incoming.method.toString, incoming.headers, incoming.queryString, incoming.body).asInstanceOf[AnyRef]))) |@|
       matchStr((s"${build.analysisResult.namespace.path}/${build.analysisResult.namespace.name}/${build.analysisResult.webhook.req.uri.map(_.trimSlashes).getOrElse("")}").trimSlashes.toLowerCase, incoming.uri.map(_.trimSlashes.toLowerCase).getOrElse("")) |@|
       matchMap(build.analysisResult.webhook.req.headers, incoming.headers) |@|
       matchMap(build.analysisResult.webhook.req.queryString, incoming.queryString) |@|
       matchBody(build.analysisResult.webhook.req.body, incoming.body))(_ |+| _ |+| _ |+| _ |+| _).
       map(variables => (build, variables))
    }).flatten

    matchedBuilds.map(b => {
      evaluatorActor.tAsk[EvaluationResponse](EvaluationRequest(b._1.className, b._1.bytes, b._2)).map(r => SuccessfulRun(b._1, r.result)).handle {
        case _ => FailedRun(b._1)
      }
    }).sequenceU.map(RunResult(_))
  }
}

object Executor {
  val compilerPositionAmendment = 93

  case class Build(analysisResult: AnalysisResult, className: String, bytes: List[Byte])

  sealed trait Run
  case class SuccessfulRun(build: Build, result: Any) extends Run
  case class FailedRun(build: Build) extends Run
  case class RunResult(runs: List[Run])
}