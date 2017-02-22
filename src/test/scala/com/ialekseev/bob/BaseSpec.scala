package com.ialekseev.bob

import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}

trait BaseSpec extends WordSpecLike with Matchers with MockitoSugar with BeforeAndAfterEach with BeforeAndAfterAll
