package com.ialekseev.bob.run

import akka.actor.{Actor, ActorRef}
import scalaz.effect.IO
import scalaz.concurrent.Task
import delorean._
import akka.util.Timeout
import akka.pattern.ask
import akka.pattern.pipe
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

object TaskConversions {
  implicit class IOWrapper[T](io: IO[T]){
    def toTask: Task[T] = Task.delay(io.unsafePerformIO())
  }

  implicit class ActorRefWrapper(actorRef: ActorRef)(implicit timeout: Timeout) {
    def tAsk[T](message: Any)(implicit tag: ClassTag[T], ec: ExecutionContext): Task[T] = (actorRef ? message).mapTo[T].toTask
  }

  implicit class TaskWrapper[T](task: Task[T]) {
    def toFuture(): Future[T] = task.unsafeToFuture()
    def pipeTo(recipient: ActorRef)(implicit sender: ActorRef = Actor.noSender, ec: ExecutionContext) = task.unsafeToFuture() pipeTo recipient
  }
}
