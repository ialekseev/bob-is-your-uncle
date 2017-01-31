package com.ialekseev

import scala.concurrent.{Future, Promise}
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

package object bob {
  def unsafeToScala[A](task: Task[A]): Future[A] = {
    val p = Promise[A]
    task.unsafePerformAsync {
      case \/-(a) => p.success(a)
      case -\/(t) => p.failure(t)
    }
    p.future
  }
}
