package com.ialekseev.bob

import com.ialekseev.bob.analyzer.Analyzer.Namespace

package object dsl {
  def console(obj: Any)(implicit namespace: Namespace) = {
    println(Console.MAGENTA + s"${namespace.path}#${namespace.name}: $obj" + Console.RESET)
  }
}
