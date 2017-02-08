package com.ialekseev.bob.run

import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}

package object cli {
  trait BaseCommand extends IoShared {
    val exec = new Executor {
      val analyzer = DefaultAnalyzer
      val scalaCompiler = compiler
    }

    def compiler: ScalaCompiler
  }
}
