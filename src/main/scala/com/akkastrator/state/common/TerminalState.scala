package com.akkastrator.state.common

object TerminalState {

  val END: String = "__END__"

  case class Succeed() extends State("Succeed", None, true)

  case class Fail(error: String, cause: String) extends State("Fail", None, true)

}
