package com.ialekseev.bob.run

import java.nio.file.Paths
import akka.actor.Actor
import com.ialekseev.bob.run.SourceStateActor.{ReadSourcesRequestMessage, ReadSourcesResponseMessage, SaveSourcesRequestMessage, SaveSourcesResponseMessage}
import com.ialekseev.bob.run.TaskConversions._
import scala.concurrent.ExecutionContext

class SourceStateActor(implicit executionContext: ExecutionContext) extends Actor with IoShared {
  override def receive: Receive = {
    case ReadSourcesRequestMessage(dirs) => readSources(dirs.map(Paths.get(_))).map(ReadSourcesResponseMessage(_)).pipeTo(sender)
    case SaveSourcesRequestMessage(inputDirs) => saveSources(inputDirs).map(_ => SaveSourcesResponseMessage).pipeTo(sender)
  }
}

object SourceStateActor {
  case class ReadSourcesRequestMessage(dirs: List[String])
  case class ReadSourcesResponseMessage(inputDirs: List[InputDir])

  case class SaveSourcesRequestMessage(inputDirs: List[InputDir])
  case object SaveSourcesResponseMessage
}