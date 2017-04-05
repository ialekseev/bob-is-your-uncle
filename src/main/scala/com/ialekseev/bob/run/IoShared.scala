package com.ialekseev.bob.run

import java.nio.file.{Files, Path, Paths}
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.exec.analyzer.Analyzer.AnalysisResult
import scala.io.StdIn
import scala.language.reflectiveCalls
import scala.collection.JavaConverters._
import scalaz.Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.concurrent.Task
import scalaz.{-\/, \/, \/-}
import fs2.interop.scalaz._
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

trait IoShared {

  val defaultSourcesLocation = Paths.get("bobs")
  val varsFileName = Paths.get("_vars.json")
  val fileExtension = ".bob"

  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = try { f(param) } finally { param.close() }

  def getFilePath(dir: Path, fileName: String): Path = {
    require(Files.isDirectory(dir))

    Paths.get(dir.toString, fileName)
  }

  def getFileNameWithoutExtension(filePath: Path): String = {
    val fileName = filePath.getFileName.toString
    val pos = fileName.lastIndexOf(".")
    if (pos > 0) fileName.substring(0, pos) else fileName
  }

  def getVarsFilePath(dir: Path): Path = {
    require(Files.isDirectory(dir))

    getFilePath(dir, varsFileName.toString)
  }

  def listSourceFiles(dir: Path): Task[List[Path]] = {
    require(Files.isDirectory(dir))

    Task {
      using(Files.newDirectoryStream(dir))(_.iterator().asScala.filter(p => !Files.isDirectory(p) && p.getFileName.toString.endsWith(fileExtension)).toList)
    }
  }

  def readFile(filePath: Path): Task[String] = {
    require(Files.isRegularFile(filePath))

    fs2.io.file.readAll[Task](filePath, 4096).through(fs2.text.utf8Decode).through(fs2.text.lines).intersperse("\n").runFold("")(_ + _)
  }

  def findVarsFile(dir: Path): Task[Option[Path]] = {
    require(Files.isDirectory(dir))

    Task {
      val varsFilePath = getVarsFilePath(dir)
      Files.exists(varsFilePath) option  varsFilePath
    }
  }

  def readSource(sourceFilePath: Path): Task[InputSource] = {
    require(Files.isRegularFile(sourceFilePath))

    readFile(sourceFilePath).map(content => InputSource(getFileNameWithoutExtension(sourceFilePath), content))
  }

  def extractVarsFromVarsFile(varsFilePath: Path): Task[List[Variable[String]]] = {
    require(Files.isRegularFile(varsFilePath))

    import org.json4s._
    import org.json4s.native.JsonMethods._
    implicit val formats = org.json4s.DefaultFormats

    for {
      text <- readFile(varsFilePath)
      vars <- Task(parse(text).extract[Map[String, String]].map(t => Variable(t._1, t._2)).toList)
    } yield vars
  }

  def extractVarsForDir(dir: Path): Task[List[Variable[String]]] = {
    require(Files.isDirectory(dir))

    for {
      varsFile <- findVarsFile(dir)
      vars <- varsFile match {
        case Some(f) => extractVarsFromVarsFile(f)
        case None => Task.now(List.empty)
      }
    } yield vars
  }

  def extractVarsForFile(filePath: Path): Task[List[Variable[String]]] = {
    require(Files.isRegularFile(filePath))

    extractVarsForDir(filePath.getParent)
  }

  def readSources(dirs: List[Path]): Task[List[InputDir]] = {
    require(dirs.nonEmpty)
    require(dirs.forall(Files.isDirectory(_)))

    dirs.map(dir => {
      for {
        sourceFiles <- listSourceFiles(dir)
        vars <- extractVarsForDir(dir)
        inputSources <- sourceFiles.map(sourceFilePath => readSource(sourceFilePath)).sequenceU
      } yield InputDir(dir.toString, inputSources, vars)
    }).sequenceU
  }

  def delete(filePath: Path): Task[Boolean] = Task(Files.deleteIfExists(filePath))

  def saveFile(filePath: Path, content: String): Task[Unit] = {
    require(content.nonEmpty)

    for {
      _ <- delete(filePath)
      _ <- fs2.Stream.eval(Task.now(content)).through(fs2.text.utf8Encode).through(fs2.io.file.writeAll(filePath)).run
    } yield (): Unit
  }

  def saveVarsForDir(dir: Path, vars: List[Variable[String]]): Task[Unit] = {
    require(Files.isDirectory(dir))

    val content = pretty(render((vars.foldLeft(JObject())((js, v) => js ~ (v.name -> v.value)))))
    saveFile(getVarsFilePath(dir), content)
  }

  def saveSources(dirs: List[InputDir]): Task[Unit] = {

    dirs.map(inputDir => {
      val dir = Paths.get(inputDir.path)
      assume(Files.isDirectory(dir), "The Input Directory you've specified doesn't look like a directory")

      for {
        _ <- listSourceFiles(dir).flatMap(sources => sources.map(delete(_)).sequenceU)
        _ <- inputDir.sources.map(inputSource => saveFile(Paths.get(dir.toString, inputSource.name + fileExtension), inputSource.content)).sequenceU
        _ <- saveVarsForDir(dir, inputDir.vars)
      } yield (): Unit
    }).sequenceU.map(_ => ())
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
