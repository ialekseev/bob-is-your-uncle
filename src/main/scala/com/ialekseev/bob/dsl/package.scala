package com.ialekseev.bob

package object dsl {
  object Console {
    def write(obj: Any) = println(obj)
  }
}
