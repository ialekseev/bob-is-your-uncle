package com.ialekseev.bob

import scalaz.{EitherT}

package object run {
  case class InputSource(path: String, content: String, vars: List[(String, String)])
}
