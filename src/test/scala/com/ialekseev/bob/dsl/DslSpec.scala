package com.ialekseev.bob.dsl

import com.ialekseev.bob.BaseSpec

class DslSpec extends BaseSpec {

  "Playing with dsl" when {

    "dsl has several steps" should {
      "produce result with logs" in {
        //act
        val result = action("action1") { r =>
                      ("hello", 100)
                    } ~
                    next("action2") { n =>
                      n._1.length + n._2
                    }

        //assert
        result should be (ActionResult(105, Vector(ActionLog("action1", "(hello,100)"), ActionLog("action2", "105"))))
      }
    }
  }
}
