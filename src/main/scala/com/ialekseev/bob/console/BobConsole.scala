package com.ialekseev.bob.console

import scalaz.Scalaz._

trait BobConsole {
  this: Command =>

  def showTitleMessage(message: String) = {
    println(Console.CYAN +  "  +-------------------------------+")
    println(Console.CYAN + s"  | $message")
    println(Console.CYAN +  "  +-------------------------------+")
    println()
  }

  def showSuccess(message: String) = {
    println(Console.GREEN + s"  $message")
  }

  def showError(source: String, startOffset: Int, endOffset: Int) = {
    println(Console.RED + s"  Error positions: from $startOffset} to $endOffset}" + Console.RESET)
  }

  def showError(source: String, offset: Int, message: String) = {
    def showErrorPosition() = {
      println(Console.RED + s"  Error position: ${errorCoordinate(source, offset)}. Message: $message" + Console.RESET)
    }

    def showErrorContext() = {
      val after = if (offset + 1 < source.length - 1) some(source.substring(offset + 1)) else none
      val context = (source.substring(0, offset), source(offset), after)
      println(Console.RED + "  [" + Console.RESET)
      println(context._1 + Console.RED + context._2 + Console.RESET + context._3.getOrElse(""))
      println(Console.RED + "  ]" + Console.RESET)
    }

    showErrorPosition()
    showErrorContext()
  }

  def showError(message: String, e: Throwable) = {
    println(Console.RED + s"  $message. Internal error: $e" + Console.RESET)
  }
}
