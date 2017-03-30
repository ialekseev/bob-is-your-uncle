package com.ialekseev.bob.run

import java.nio.file.{FileSystems, Files, Paths}
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.exec.analyzer.Analyzer.AnalysisResult
import scala.io.{StdIn}
import scala.language.reflectiveCalls
import scala.collection.JavaConverters._
import scalaz.Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.concurrent.Task
import scalaz.{-\/, \/, \/-}
import fs2.interop.scalaz._

trait IoShared {

  val defaultSourcesLocation = "bobs"
  val varsFileName = "_vars.json"
  val fileExtension = ".bob"

  def normalizeDirPath(dir: String): String = {
    require(dir.nonEmpty)

    val separator = FileSystems.getDefault.getSeparator
    if (dir.endsWith(separator)) dir else dir + separator
  }

  def getVarsFilePath(dir: String): String = {
    require(dir.nonEmpty)

    normalizeDirPath(dir) + varsFileName
  }

  def listFiles(dir: String): Task[List[String]] = {
    require(dir.nonEmpty)

    Task {
      Files.newDirectoryStream(Paths.get(dir)).iterator().asScala.map(_.toAbsolutePath.toString).toList
    }
  }

  def listSourceFiles(dir: String): Task[List[String]] = {
    require(dir.nonEmpty)

    for {
      files <- listFiles(dir)
      sourceFiles <- Task(files.filter(_.endsWith(fileExtension)))
    } yield sourceFiles
  }

  def findVarsFile(dir: String): Task[Option[String]] = {
    require(dir.nonEmpty)

    Task {
      val varsFilePath = getVarsFilePath(dir)
      Files.exists(Paths.get(varsFilePath)) option  varsFilePath
    }
  }

  def readFile(filePath: String): Task[String] = {
    require(filePath.nonEmpty)

    fs2.io.file.readAll[Task](Paths.get(filePath), 4096).through(fs2.text.utf8Decode).through(fs2.text.lines).intersperse("\n").runFold("")(_ + _)
  }

  def deleteIfExists(filePath: String): Task[Boolean] = {
    require(filePath.nonEmpty)

    Task(Files.deleteIfExists(Paths.get(filePath)))
  }

  def updateFile(filePath: String, content: String): Task[Unit] = {
    require(filePath.nonEmpty)
    require(content.nonEmpty)

    for {
      _ <- deleteIfExists(filePath)
      _ <- fs2.Stream.eval(Task.now(content)).through(fs2.text.utf8Encode).through(fs2.io.file.writeAll(Paths.get(filePath))).run
    } yield (): Unit
  }

  def extractVarsFromVarsFile(varsFilePath: String): Task[List[Variable[String]]] = {
    require(varsFilePath.nonEmpty)

    import org.json4s._
    import org.json4s.native.JsonMethods._
    implicit val formats = org.json4s.DefaultFormats

    for {
      text <- readFile(varsFilePath)
      vars <- Task(parse(text).extract[Map[String, String]].map(t => Variable(t._1, t._2)).toList)
    } yield vars
  }

  def extractVarsForDir(dir: String): Task[List[Variable[String]]] = {
    require(dir.nonEmpty)

    for {
      varsFile <- findVarsFile(dir)
      vars <- varsFile match {
        case Some(f) => extractVarsFromVarsFile(f)
        case None => Task.now(List.empty)
      }
    } yield vars
  }

  def extractVarsForFile(filePath: String): Task[List[Variable[String]]] = {
    require(filePath.nonEmpty)

    extractVarsForDir(Paths.get(filePath).getParent.toAbsolutePath.toString)
  }

  def readSource(sourceFilePath: String): Task[InputSource] = {
    require(sourceFilePath.nonEmpty)

    readFile(sourceFilePath).map(content => InputSource(sourceFilePath, content))
  }

  def readSources(dirs: List[String]): Task[List[InputDir]] = {
    require(dirs.nonEmpty)

    dirs.map(dir => {
      for {
        sourceFiles <- listSourceFiles(dir)
        vars <- extractVarsForDir(dir)
        inputSources <- sourceFiles.map(sourceFilePath => readSource(sourceFilePath)).sequenceU
      } yield InputDir(dir, inputSources, vars)
    }).sequenceU
  }

  def show(message: String = ""): IO[Unit] = putStrLn(message)
  def read(message: String = ""): IO[String] = IO { StdIn.readLine(message)}

  def showResult(filename: String, source: String, result: BuildFailed \/ Build): IO[Unit] = {
    require(!filename.isEmpty)
    require(!source.isEmpty)

    def showSuccess(filename: String, r: AnalysisResult): IO[Unit] = {
      for {
        _ <- show()
        _ <- showFileName(filename)
        _ <- show(Console.WHITE + "namespace: " + r.namespace + Console.RESET)
        _ <- show(Console.WHITE + "description: "+ r.description + Console.RESET)
        _ <- show(Console.WHITE + "result: " + Console.GREEN + s"OK" + Console.RESET)
        _ <- show()
      } yield ()
    }

    result match {
      case \/-(built) => showSuccess(filename, built.analysisResult)
      case -\/(error) => error match {
        case LexicalAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, "Unexpected token or block")
        case SyntaxAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case SemanticAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case CompilationFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
      }
    }
  }

  def showError(filename: String, source: String, startOffset: Int, endOffset: Int, message: String): IO[Unit] = {
    require(!filename.isEmpty)
    require(!source.isEmpty)
    require(startOffset >= 0)
    require(endOffset >= 0)

    def showErrorContext(source: String, startOffset: Int, endOffset: Int): IO[Unit] = {
      val before = source.substring(0, startOffset)
      val error = {
        val err = source.substring(startOffset, endOffset + 1)
        if (err.matches("\\s+")) "_" * err.length
        else err
      }
      val after = if (endOffset + 1 < source.length - 1) some(source.substring(endOffset + 1)) else none

      val context = (before, error, after)
      for {
        _ <- show(Console.RED + "[" + Console.RESET)
        _ <- show(Console.WHITE + context._1 + Console.RED + context._2 + Console.WHITE + context._3.getOrElse("") + Console.RESET)
        _ <- show(Console.RED + "]" + Console.RESET)
      } yield ()
    }

    def errorCoordinate(source: String, offset: Int): (Int, Int) = {
      require(offset >= 0)

      if (source.isEmpty || offset == 0) (1, 1)
      else {
        val beforeOffset = source.take(offset)
        val nlIndex = beforeOffset.reverse.indexWhere(_ == '\n')

        val column = if (nlIndex >= 0) nlIndex + 1 else offset + 1
        val line = beforeOffset.count(_ == '\n') + 1
        (line, column)
      }
    }

    for {
      _ <- show()
      _ <- showFileName(filename)
      _ <- show(Console.RED + s"Error position: ${errorCoordinate(source, startOffset)}" + Console.RESET)
      _ <- show(Console.RED + s"Message: $message" + Console.RESET)
      _ <- showErrorContext(source, startOffset, endOffset)
      _ <- show()
    } yield ()
  }

  def showError(message: String, errors: Throwable*): IO[Unit] = {
    errors.toList.map(e => showError(s"$message. Internal error: $e")).sequenceU.map(_ => ())
  }

  def showError(message: String): IO[Unit] = {
    require(message.nonEmpty)

    for {
      _ <- show()
      _ <- show(Console.RED + message + Console.RESET)
      _ <- show()
    } yield ()
  }

  def showFileName(filename: String): IO[Unit] = {
    require(filename.nonEmpty)

    show(Console.CYAN + "[" + filename + "]" + Console.RESET)
  }
}
