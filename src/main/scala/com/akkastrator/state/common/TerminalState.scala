package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.akkastrator.state.common.State.State
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}

object TerminalState {

  trait TerminalState

  val END: String = "__END__"

  case class Succeed(inputPath: JsonPath = State.CONTEXT_ROOT, outputPath: JsonPath = State.CONTEXT_ROOT)
    extends State("Succeed") with TerminalState with Input with Output {
    override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = {
      val effectiveInput = getInput(context)
      Future.successful((TerminalState.END, getOutput(effectiveInput)))
    }
  }

  case class Fail(error: String, cause: String)
    extends State("Fail") with TerminalState {
    override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = {
      Future.failed(StateException.StateFailure(error, cause))
    }
  }

}
