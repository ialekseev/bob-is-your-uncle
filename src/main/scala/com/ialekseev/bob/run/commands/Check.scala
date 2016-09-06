package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.{Command}

trait Check {
  this: Command =>

  def checkCommand(filename: String) = {
    readSource(filename).map(source => {
      showResult(filename, source, exec.build(source).unsafePerformSync)
    }).recover { case e => showError("Can't read the file provided", e) }
  }
}
