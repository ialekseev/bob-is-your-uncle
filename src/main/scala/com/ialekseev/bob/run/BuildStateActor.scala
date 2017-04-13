package com.ialekseev.bob.run

import akka.actor.Actor
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run.BuildStateActor.{GetBuildStateRequestMessage, GetBuildStateResponseMessage, SetBuildStateRequestMessage, SetBuildStateResponseMessage}

class BuildStateActor extends Actor {
  var builds: List[Build] = List.empty
  override def receive: Receive = {
    case SetBuildStateRequestMessage(builds) => {
      this.builds = builds
      sender() ! SetBuildStateResponseMessage
    }
    case GetBuildStateRequestMessage => sender() ! GetBuildStateResponseMessage(builds)
  }
}

object BuildStateActor {
  case class SetBuildStateRequestMessage(builds: List[Build])
  case object SetBuildStateResponseMessage

  case object GetBuildStateRequestMessage
  case class GetBuildStateResponseMessage(builds: List[Build])
}
