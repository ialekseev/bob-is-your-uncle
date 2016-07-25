package com.ialekseev.bob

import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpecLike}

trait BaseSpec extends WordSpecLike with Matchers with MockitoSugar with BeforeAndAfterEach
