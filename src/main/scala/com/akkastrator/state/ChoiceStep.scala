package com.akkastrator.state

import com.akkastrator.state.common.{Input, Output, Step}
import com.akkastrator.state.conditions.Choices.TopLevelChoice
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


case class ChoiceStep(choices: List[TopLevelChoice],
                      default: Option[String],
                      inputPath: JsonPath = Step.CONTEXT_ROOT,
                      outputPath: JsonPath = Step.CONTEXT_ROOT)
  extends Step("Choice") with Input with Output {

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
