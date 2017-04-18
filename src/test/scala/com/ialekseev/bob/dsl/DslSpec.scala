package com.ialekseev.bob.dsl

import com.ialekseev.bob._
import com.ialekseev.bob.BaseSpec
import org.json4s.JObject
import org.json4s.JsonDSL._

class DslSpec extends BaseSpec {

  "Dsl" when {
    val run = dsl.run

    "dsl" should {
      "dsl" in {

        action("action1") { r =>
          ("name" -> "joe") ~ ("age" -> Some(35))
        } ~
        next[JObject]("next") { x =>
          ("name" -> "joe") ~ ("age" -> x.toString)
        } ~
        next[JObject]("next") { x =>
          ("name" -> "joe") ~ ("age" -> "111")
        } ~
        run
      }
    }
  }
}
