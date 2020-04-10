package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}

object TerminalStep {
  val END: String = "__END__"

  case class Succeed(inputPath: JsonPath = Step.CONTEXT_ROOT, outputPath: JsonPath = Step.CONTEXT_ROOT)
    extends Step("Succeed") with Input with Output {
    override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = {
      val effectiveInput = getInput(context)
      Future.successful((TerminalStep.END, getOutput(effectiveInput)))
    }
  }

  case class Fail(error: String, cause: String)
    extends Step("Fail") {
    override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = {
      Future.failed(StateException.StateFailure(error, cause))
    }
  }

}
