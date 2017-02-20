package com.ialekseev.bob.run

import com.ialekseev.bob.exec.{Executor}

package object cli {
  trait BaseCommand extends IoShared {
    val exec: Executor
  }
}
