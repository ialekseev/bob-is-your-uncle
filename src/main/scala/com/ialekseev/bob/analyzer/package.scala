package com.ialekseev.bob

import scalaz.Scalaz._

package object analyzer {
  val SOT = '\u0002' //Start of Text character
  val EOT = '\u0003' //End of Text character

  def someUnit = some((): Unit)
  def unit = (): Unit

  implicit class StringWrapper(str: String){
    require(str.length > 1)
    def dismantle2: (Char, String) = (str(0), str.substring(1))
    def dismantle3: (Char, String, Char) = (str(0), str.substring(1, str.length - 1), str(str.length - 1))
  }
}
