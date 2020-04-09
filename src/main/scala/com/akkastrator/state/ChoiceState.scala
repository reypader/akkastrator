package com.akkastrator.state

import com.akkastrator.state.ChoiceState.TopLevelChoice
import com.akkastrator.state.common.State.State
import com.akkastrator.state.common.{Input, NextStep, Output, State}
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ChoiceState {

  trait ChoiceRule {
    def evaluate(context: State#Context): Boolean
  }

  trait Comparison[T] {
    def comparableValue: T
  }

  trait VariableAccess[T] {
    def getActualValue(context: State#Context, variable: JsonPath): T
  }

  trait TopLevelChoice extends ChoiceRule with NextStep


}


case class ChoiceState(choices: List[TopLevelChoice],
                       default: Option[String],
                       inputPath: JsonPath = State.CONTEXT_ROOT,
                       outputPath: JsonPath = State.CONTEXT_ROOT)
  extends State("Choice") with Input with Output {

  if (choices.length < 1) {
    throw new IllegalArgumentException("Must have at least one choice")
  }

  override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] =
    Future.fromTry(Try {
      val effectiveInput = getInput(context)
      val result = choices.find(rule => rule.evaluate(effectiveInput)).map(rule => rule.next)
      if (result.isEmpty && default.isEmpty) {
        throw StateException.UnresolvableChoiceException(this, context)
      } else {
        (result.getOrElse(default.get), getOutput(context))
      }
    })
}
