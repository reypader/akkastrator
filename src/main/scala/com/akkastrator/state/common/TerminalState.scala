package com.akkastrator.state.common

object TerminalState {

  trait TerminalState

  val END: String = "__END__"

  case class Succeed() extends State("Succeed") with TerminalState

  case class Fail(error: String, cause: String) extends State("Fail") with TerminalState

}
