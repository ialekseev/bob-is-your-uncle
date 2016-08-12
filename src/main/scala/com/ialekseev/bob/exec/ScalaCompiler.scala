package com.ialekseev.bob.exec

import com.ialekseev.bob.{CompilationFailed, CompilationError}
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.{Random, Try}
import scalaz._
import Scalaz._

class ScalaCompiler {
  val compilerPositionAmendment = 90
  val compiler = new Compiler(None, ListBuffer.empty)

  def compile(code: String): CompilationFailed \/ Class[_] = {
    require(!code.isEmpty)

    def amend(pos: Int) = pos - compilerPositionAmendment

    synchronized {
      Try(compiler.compile(code)).map(_.right).getOrElse {
        val errorsAmended = compiler.reportedErrors.map(e => e.copy(startOffset = amend(e.startOffset), pointOffset = amend(e.pointOffset), endOffset = amend(e.endOffset)))
        CompilationFailed(errorsAmended).left
      }
    }
  }
}

//https://eknet.org/main/dev/runtimecompilescala.html + custom reporter (todo: this is a thread-unsafe head-on implementation)
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

  /**Compiles the code as a class into the class loader of this compiler.
    *
    * @param code
    * @return
    */
  def compile(code: String): Class[_] = {
    reporter.reset()
    val run = new global.Run
    //val className =  classNameForCode(code)
    val className = "bob" + Random.alphanumeric.take(40).mkString
    val sourceFiles = List(new BatchSourceFile("(inline)", wrapCodeInClass(className, code)))
    run.compileSources(sourceFiles)
    classLoader.loadClass(className)
  }

  /** Compiles the source string into the class loader and
    * evaluates it.
    *
    * @param code
    * @tparam T
    * @return
    */
  def eval[T](code: String): T = {
    val cls = compile(code)
    cls.getConstructor().newInstance().asInstanceOf[() => Any].apply().asInstanceOf[T]
  }

  /*protected def classNameForCode(code: String): String = {
    val digest = MessageDigest.getInstance("SHA-1").digest(code.getBytes)
    "sha"+new BigInteger(1, digest).toString(16)
  }*/

  /*
  * Wrap source code in a new class with an apply method.
  */
  private def wrapCodeInClass(className: String, code: String) = {
    "class " + className + " extends (() => Any) {\n" +
      "  def apply() = {\n" +
      code + "\n" +
      "  }\n" +
      "}\n"
  }
}