package com.ialekseev.bob.exec

import com.ialekseev.bob.{CompilationFailed, CompilationError}
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.{Random}
import scalaz._
import Scalaz._

class ScalaCompiler {
  val compiler = new Compiler(None, ListBuffer.empty)

  def compile(code: String, fields: String = ""): CompilationFailed \/ String = {
    require(!code.isEmpty)

    synchronized {
      val className = compiler.compile(code, fields)
      if (compiler.reportedErrors.length == 0) className.right
      else CompilationFailed(compiler.reportedErrors).left
    }
  }

  def eval[T](className: String, variables: Seq[(String, String)]): T = {
    synchronized {
      compiler.eval(className, variables)
    }
  }
}

//https://eknet.org/main/dev/runtimecompilescala.html tuned for our needs + custom reporter (todo: this is a thread-unsafe head-on implementation)
import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import tools.nsc.io.{VirtualDirectory, AbstractFile}
import java.io.File

private[exec] class Compiler(targetDir: Option[File], val reportedErrors: ListBuffer[CompilationError]) {

  val target = targetDir match {
    case Some(dir) => AbstractFile.getDirectory(dir)
    case None => new VirtualDirectory("(memory)", None)
  }

  val customSettings = {
    val dependencies = List(
      "org.scala-lang/scala-library/jars/scala-library-2.11.8.jar"
    )
    val s = new Settings()
    s.outputDirs.setSingleOutput(target)
    val homePath = System.getProperty("user.home")
    val classPath = dependencies.map(homePath + "\\.ivy2\\cache\\" + _)
    s.classpath.value = classPath.mkString(File.pathSeparator)
    s
  }

  private val reporter = new AbstractReporter {
    val settings: Settings = customSettings
    override def reset(): Unit = {
      super.reset()
      reportedErrors.clear()
    }
    def displayPrompt(): Unit = ???
    def display(pos: Position, msg: String, severity: Severity): Unit = {
      reportedErrors += CompilationError(pos.start, pos.point, pos.end, msg)
    }
  }

  private val global = new Global(customSettings, reporter)
  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  def compile(code: String, fields: String): String = {
    reporter.reset()
    val run = new global.Run
    val className = "bob" + Random.alphanumeric.take(40).mkString
    val test = wrapCodeInClass(className, code, fields)
    val sourceFiles = List(new BatchSourceFile("(inline)", test))
    run.compileSources(sourceFiles)
    className
  }

  def eval[T](className: String, variables: Seq[(String, String)]): T = {
    val cls = classLoader.loadClass(className)
    val instance = cls.getConstructor().newInstance()
    variables.foreach(v => {
      instance.getClass.getMethods.find(_.getName == v._1 + "_$eq").get.invoke(instance, v._2.asInstanceOf[AnyRef])
    })
    instance.asInstanceOf[() => Any].apply().asInstanceOf[T]
  }

  private def wrapCodeInClass(className: String, code: String, fields: String) = {
    val fieldsLine = if (fields.nonEmpty) fields + "\n" else fields
    "class " + className + " extends (() => Any) {\n" +
      fieldsLine +
      "  def apply() = {\n" +
      code + "\n" +
      "  }\n" +
      "}\n"
  }
}