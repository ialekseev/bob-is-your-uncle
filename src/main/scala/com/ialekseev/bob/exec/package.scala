package com.ialekseev.bob

import scala.util.matching.Regex

package object exec {

  private val trimSlashesPattern = new Regex("^/*(.*[^/])/*$")

  implicit class StringWrapper(str: String){
    def trimSlashes: String = {
      val s = str.replace("^/*$", "")
      if (s.isEmpty) s
      else trimSlashesPattern.findFirstMatchIn(s).map(m => m.group(1)).getOrElse(sys.error("Unexpected string for trimming slashes"))
    }
  }
}
