package com.ialekseev.bob

package object run {
  case class InputSource(path: String, content: String, vars: List[(String, String)])
}
