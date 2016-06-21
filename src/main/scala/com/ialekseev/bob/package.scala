package com.ialekseev

import scalaz.Scalaz._

package object bob {
  val SOT = '\u0002' //Start of Text character
  val EOT = '\u0003' //End of Text character

  def someUnit = some((): Unit)
  def unit = (): Unit
}
