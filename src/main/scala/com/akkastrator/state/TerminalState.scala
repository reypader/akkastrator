package com.akkastrator.state

import com.akkastrator.state.common.State

object TerminalState {

  case class Succeed() extends State("Succeed", true)

  case class Fail(error: String, cause: String) extends State("Fail", true)

}
