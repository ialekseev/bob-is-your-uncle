package com.ialekseev.bob

import com.ialekseev.bob.run.commands.{Service, Shell, Check}
import com.ialekseev.bob.run.{Command}
import scala.io.{Codec, StdIn}
import scala.io.Source
import scala.util.Try

object Boot extends App with Command with Check with Shell with Service {

  def show(message: String = "") = println(message)
  def read(message: String = ""): String = StdIn.readLine(message)

  def readSource(filename: String): Try[String] = {
    def normalizeSource(source: String): String = {
      source.replaceAll("\r\n", "\n")
    }
    Try {
      val fileToCheck = Source.fromFile(filename)(Codec.UTF8)
      val source = normalizeSource(fileToCheck.mkString)
      fileToCheck.close()
      source
    }
  }

  def readSources(dir: String): Try[List[(String, String)]] = {
    for {
      sourceFiles <- Try(new java.io.File(defaultBuildsLocation).listFiles.filter(_.getName.endsWith(fileExtension)))
      sources <- Try(sourceFiles.map(file => (file.getPath, readSource(file.getPath).get)).toList)
    } yield sources
  }

  case class Config(check: String = "", shell: Boolean = false, service: Boolean = false)
  val parser = new scopt.OptionParser[Config]("bob") {
    opt[String]("check").action((x, c) => c.copy(check = x)).text("check - is a path to file")
    opt[Unit]("shell").action((_, c) => c.copy(shell = true)).text("shell - enter the shell")
    opt[Unit]("service").action((_, c) => c.copy(service = true)).text("service - run the http service")
  }

  parser.parse(args, Config()) match {
    case Some(Config(path, _, _)) if path.nonEmpty => checkCommand(path)
    case Some(Config(_, true, _)) => shellCommand()
    case Some(Config(_, _, true)) => serviceCommand()
    case None =>
  }
}
