package com.ialekseev.bob.exec

import java.io.File
import java.net.{URLClassLoader}
import java.nio.file.{Paths}
import akka.actor.Actor
import com.ialekseev.bob.CompilationError
import com.ialekseev.bob.exec.Compiler._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile, ScalaClassLoader, Position}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.{Random}
import scalaz.Scalaz._
import scalaz.\/

object Compiler {
  case class CompilationRequest(code: String, imports: String = "", fields: String = "", implicits: String = "")
  case class CompilationSucceededResponse(className: String, bytes: List[Byte])
  case class CompilationFailedResponse(r: List[CompilationError])
  case class EvaluationRequest(className: String, bytes: List[Byte], variables: List[(String, AnyRef)])
  case class EvaluationResponse(result: Any)
}

class CompilerActor extends Actor {
  val target = new VirtualDirectory("(memory)", None)
  val reportedErrors: ListBuffer[CompilationError] = ListBuffer.empty

  val customSettings = {
    val classPath = this.getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.toList.map(f => Paths.get(f.toURI()).toFile().getPath)
    val s = new Settings()
    s.outputDirs.setSingleOutput(target)
    s.classpath.value = classPath.mkString(File.pathSeparator)
    s
  }

  val reporter = new AbstractReporter {
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

  val global = new Global(customSettings, reporter)
  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  def generateRandomName: String = "bob" + Random.alphanumeric.take(40).mkString

  def wrapCodeInClass(className: String, code: String, imports: String, fields: String, implicits: String) = {
    "class " + className + " extends (() => Any) {\n" +
      imports + "\n" +
      fields + "\n" +
      implicits + "\n" +
      "  def apply() = {\n" +
      code + "\n" +
      "  }\n" +
      "}\n"
  }

  def compile(code: String, imports: String, fields: String, implicits: String): List[CompilationError] \/ (String, List[Byte]) = {
    require(code.nonEmpty)

    reporter.reset()
    target.clear()

    val run = new global.Run
    val className = generateRandomName
    val wrappedCodeInClass = wrapCodeInClass(className, code, imports, fields, implicits)
    val sourceFiles = List(new BatchSourceFile(className, wrappedCodeInClass))
    run.compileSources(sourceFiles)

    if (reportedErrors.length == 0) (className, classLoader.classBytes(className).toList).right else reportedErrors.toList.left
  }

  override def receive: Receive = {
    case CompilationRequest(code, imports, fields, implicits) => {
      val result = compile(code, imports, fields, implicits)
      result.map(r => sender ! CompilationSucceededResponse(r._1, r._2)).leftMap(er => sender ! CompilationFailedResponse(er))
    }
  }
}

class EvaluatorActor extends Actor {
  import scala.language.reflectiveCalls

  val classLoader = new ClassLoader(this.getClass.getClassLoader) with ScalaClassLoader  {
    def defineClass(name: String, bytes: Array[Byte]): Class[_] = {
      defineClass(name, bytes, 0, bytes.length)
    }
  }

  def eval(className: String, bytes: List[Byte], variables: List[(String, AnyRef)]): Any = {
    require(className.nonEmpty)
    require(bytes.nonEmpty)

    val cls = classLoader.defineClass(className, bytes.toArray)

    val instance = cls.getConstructor().newInstance()
    variables.foreach(v => {
      instance.getClass.getMethods.find(_.getName == v._1 + "_$eq").get.invoke(instance, v._2.asInstanceOf[AnyRef])
    })
    instance.asInstanceOf[() => Any].apply()
  }

  override def receive: Receive = {
    case EvaluationRequest(className, bytes, variables) => sender ! EvaluationResponse(eval(className, bytes, variables))
  }
}