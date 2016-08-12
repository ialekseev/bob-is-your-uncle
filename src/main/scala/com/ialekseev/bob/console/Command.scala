package com.ialekseev.bob.console

trait Command {
  def normalizeSource(source: String): String = {
    source.replaceAll("\r\n", "\n")
  }

  def errorCoordinate(source: String, offset: Int): (Int, Int) = {
    require(offset >= 0)

    if (source.isEmpty || offset == 0) (1, 1)
    else {
      val beforeOffset = source.take(offset)
      val nlIndex = beforeOffset.reverse.indexWhere(_ == '\n')

      val column = if (nlIndex > 0) nlIndex + 1 else 1
      val line = beforeOffset.count(_ == '\n') + 1
      (line, column)
    }
  }
}
