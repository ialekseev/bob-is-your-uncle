package com.ialekseev

import org.json4s.JsonAST.JValue
import scalaz.EitherT
import scala.util.Try
import scalaz.Scalaz._
import scalaz.effect.IO

package object bob {
  type IoTry[T] =  EitherT[IO, List[Throwable], T]
  object IoTry {
    def apply[T](t: => T): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(Try(t).toDisjunction.leftMap(List(_))))

    def success[T](t: T): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(t.right))
    def successIO[T](io: IO[T]): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](io.map(_.right))
    def successSwappedIO[T](io: IO[T]): EitherT[IO, T, List[Throwable]] = EitherT.eitherT[IO, T, List[Throwable]](io.map(_.left))
    def failure[T](f: Throwable): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(List(f).left))
  }

  case class LexicalError(startOffset: Int, endOffset: Int)
  case class SyntaxError(startOffset: Int, endOffset: Int, tokenIndex: Int, message: String)
  case class SemanticError(startOffset: Int, endOffset: Int, message: String)
  case class CompilationError(startOffset: Int, pointOffset: Int, endOffset: Int, message: String)

  sealed trait StageFailed
  case class LexicalAnalysisFailed(errors: Seq[LexicalError]) extends StageFailed
  case class SyntaxAnalysisFailed(errors: Seq[SyntaxError]) extends StageFailed
  case class SemanticAnalysisFailed(errors: Seq[SemanticError]) extends StageFailed
  case class CompilationFailed(errors: Seq[CompilationError]) extends StageFailed

  case class HttpRequest(uri: Option[String], method: HttpMethod.Value, headers: Map[String, String], queryString: Map[String, String], body: Option[Body]){
    require(uri.isEmpty || uri.isDefined && uri.get.nonEmpty, "Some(uri) can't be empty!")
  }

  object HttpMethod extends Enumeration {
    val GET, POST, PUT, DELETE = Value
  }

  sealed trait Body
  case class StringLiteralBody(text: String) extends Body
  case class DictionaryBody(dic: Map[String, String]) extends Body
  case class JsonBody(j: JValue) extends Body
}
