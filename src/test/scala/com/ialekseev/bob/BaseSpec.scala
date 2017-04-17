package com.ialekseev.bob

import akka.util.Timeout
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import scala.concurrent.duration._

trait BaseSpec extends WordSpecLike with Matchers with MockitoSugar with BeforeAndAfterEach with BeforeAndAfterAll {
  implicit val askTimeout = Timeout(5 seconds)
}
