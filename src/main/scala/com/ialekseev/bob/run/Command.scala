package com.ialekseev.bob.run

import com.ialekseev.bob.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}

trait Command {
  val exec = new Executor {
    val analyzer = DefaultAnalyzer
    val scalaCompiler = new ScalaCompiler
  }
}
