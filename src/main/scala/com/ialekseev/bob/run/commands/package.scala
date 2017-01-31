package com.ialekseev.bob.run

import com.ialekseev.bob.IoShared
import com.ialekseev.bob.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}

package object commands {
  trait Command extends IoShared {
    val exec = new Executor {
      val analyzer = DefaultAnalyzer
      val scalaCompiler = compiler
    }

    def compiler: ScalaCompiler
  }
}
