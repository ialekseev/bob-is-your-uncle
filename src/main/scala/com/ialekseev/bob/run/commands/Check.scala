package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.{Command}
import scala.io.Source
import scala.io.Codec

trait Check {
  this: Command =>

  def checkCommand(path: String) = {
    readSource(Source.fromFile(path)(Codec.UTF8)).map(source => {
      showResult(source, exec.build(source).unsafePerformSync)
    }).recover { case e => showError("Can't read the file provided", e) }
  }
}
