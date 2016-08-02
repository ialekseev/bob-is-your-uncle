package com.ialekseev.bob.exec

import com.ialekseev.bob.{ExecutionAnalysisFailed, ExecutionError}
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.Try
import scalaz._
import Scalaz._

class ScalaCompiler {
  val amendPosition = 90
  val compiler = new Compiler(None, ListBuffer.empty)

  def compile(code: String): ExecutionAnalysisFailed \/ Class[_] = {
    require(!code.isEmpty)

    synchronized {
      Try(compiler.compile(code)).map(_.right).getOrElse {
        val errorsAmended = compiler.reportedErrors.map(e => e.copy(startOffset = e.startOffset - amendPosition, pointOffset = e.pointOffset - amendPosition, endOffset = e.endOffset - amendPosition))
        ExecutionAnalysisFailed(errorsAmended).left
      }
    }
  }
}

//https://eknet.org/main/dev/runtimecompilescala.html + custom reporter (todo: this is a thread-unsafe head-on implementation)
import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import tools.nsc.io.{VirtualDirectory, AbstractFile}
import java.security.MessageDigest
import java.math.BigInteger
import collection.mutable
import java.io.File

private[exec] class Compiler(targetDir: Option[File], val reportedErrors: ListBuffer[ExecutionError]) {

  val target = targetDir match {
    case Some(dir) => AbstractFile.getDirectory(dir)
    case None => new VirtualDirectory("(memory)", None)
  }

  val classCache = mutable.Map[String, Class[_]]()

  private val s = new Settings()
  s.deprecation.value = true // enable detailed deprecation warnings
  s.unchecked.value = true // enable detailed unchecked warnings
  s.outputDirs.setSingleOutput(target)
  s.usejavacp.value = true

  private val reporter = new AbstractReporter {
    val settings: Settings = s
    override def reset(): Unit = {
      super.reset()
      reportedErrors.clear()
    }
    def displayPrompt(): Unit = ???
    def display(pos: Position, msg: String, severity: Severity): Unit = {
      reportedErrors += ExecutionError(pos.start, pos.point, pos.end, msg)
    }
  }

  private val global = new Global(s, reporter)
  private lazy val run = new global.Run

  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  /**Compiles the code as a class into the class loader of this compiler.
    *
    * @param code
    * @return
    */
  def compile(code: String) = {
    reporter.reset()
    val className = classNameForCode(code)
    findClass(className).getOrElse {
      val sourceFiles = List(new BatchSourceFile("(inline)", wrapCodeInClass(className, code)))
      run.compileSources(sourceFiles)
      findClass(className).get
    }
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

  def findClass(className: String): Option[Class[_]] = {
    classCache.get(className).orElse {
      try {
        val cls = classLoader.loadClass(className)
        classCache(className) = cls
        Some(cls)
      } catch {
        case e: ClassNotFoundException => None
      }
    }
  }

  protected def classNameForCode(code: String): String = {
    val digest = MessageDigest.getInstance("SHA-1").digest(code.getBytes)
    "sha"+new BigInteger(1, digest).toString(16)
  }

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