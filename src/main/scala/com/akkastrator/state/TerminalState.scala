package com.akkastrator.state

object TerminalState {

  case class Succeed() extends State("Succeed", true)

  case class Fail(error: String, cause: String) extends State("Fail", true)

}
