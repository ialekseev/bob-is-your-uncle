package com.ialekseev.bob.exec

import java.net.URLClassLoader
import java.nio.file.Paths
import akka.actor.Actor
import com.ialekseev.bob.CompilationError
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.Random


case class CompilationRequest(code: String, imports: String = "", fields: String = "", implicits: String = "")
case class CompilationSucceededResponse(r: List[Byte])
case class CompilationFailedResponse(r: List[CompilationError])
case class EvaluationRequest(code: List[Byte], variables: List[(String, AnyRef)])
case class EvaluationResponse(r: Any)

class CompilerActor extends Actor {
  val compiler = new Compiler(ListBuffer.empty)
  override def receive: Receive = {
    case CompilationRequest(code, imports, fields, implicits) => {
      val result =  compiler.compile(code, imports, fields, implicits)
      if (compiler.reportedErrors.length == 0) sender ! CompilationSucceededResponse(result.toList)
      else sender ! CompilationFailedResponse(compiler.reportedErrors.toList)
    }
  }
}

class EvaluatorActor extends Actor {
  val compiler = new Compiler(ListBuffer.empty)
  override def receive: Receive = {
    case EvaluationRequest(code, variables) => sender ! EvaluationResponse(compiler.eval(code.toArray, variables))
  }
}


//https://eknet.org/main/dev/runtimecompilescala.html tuned for our needs + custom reporter (todo: this is a thread-unsafe head-on implementation)
import java.io.File
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.{Global, Settings}

private[exec] class Compiler(val reportedErrors: ListBuffer[CompilationError]) {

  val target = new VirtualDirectory("(memory)", None)

  val customSettings = {
    val classPath =  this.getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.toList.map(f => Paths.get(f.toURI()).toFile().getPath)
    val s = new Settings()
    s.outputDirs.setSingleOutput(target)
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

  private def generateRandomName: String = "bob" + Random.alphanumeric.take(40).mkString

  //todo: refactor
  def compile(code: String, imports: String, fields: String, implicits: String): Array[Byte] = {
    reporter.reset()
    target.clear()

    val run = new global.Run
    val className = generateRandomName
    val wrappedCodeInClass = wrapCodeInClass(className, code, imports, fields, implicits)
    val sourceFiles = List(new BatchSourceFile("(inline)", wrappedCodeInClass))
    run.compileSources(sourceFiles)
    val fileName = className + ".class"
    val file = target.lookupName(fileName, false)
    file.toByteArray
  }

  //todo: refactor
  def eval(code: Array[Byte], variables: List[(String, AnyRef)]): Any = {
    target.clear()

    val className = generateRandomName
    val fileName = className + ".class"
    val file = target.fileNamed(fileName)
    val stream = file.output
    try stream.write(code) finally stream.close()

    val cls = classLoader.loadClass(className)
    val instance = cls.getConstructor().newInstance()
    variables.foreach(v => {
      instance.getClass.getMethods.find(_.getName == v._1 + "_$eq").get.invoke(instance, v._2.asInstanceOf[AnyRef])
    })
    instance.asInstanceOf[() => Any].apply()
  }

  private def wrapCodeInClass(className: String, code: String, imports: String, fields: String, implicits: String) = {
    "class " + className + " extends (() => Any) {\n" +
      imports + "\n" +
      fields + "\n" +
      implicits + "\n" +
      "  def apply() = {\n" +
      code + "\n" +
      "  }\n" +
      "}\n"
  }
}