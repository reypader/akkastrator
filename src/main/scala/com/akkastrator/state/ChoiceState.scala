package com.akkastrator.state

import com.akkastrator.state.ChoiceState.TopLevelChoice
import com.akkastrator.state.common.{Input, NextStep, Output, State}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ValueNode
import com.jayway.jsonpath.JsonPath

import scala.util.Try

object ChoiceState {

  trait ChoiceRule {
    def evaluate(context: State#Context): Boolean
  }

  trait TopLevelChoice extends ChoiceRule with NextStep

  abstract class AbstractStringEquals extends ChoiceRule {
    def stringEquals: ValueNode

    def variable: JsonPath

    def evaluate(context: State#Context): Boolean = stringEquals.equals(context.read(variable).asInstanceOf[JsonNode])
  }

  case class StringEquals(variable: JsonPath, stringEquals: ValueNode) extends AbstractStringEquals

  case class TopStringEquals(variable: JsonPath, stringEquals: ValueNode, next: String) extends AbstractStringEquals with TopLevelChoice


  abstract class AbstractNot extends ChoiceRule {
    def not: ChoiceRule

    def evaluate(context: State#Context): Boolean = !not.evaluate(context)
  }

  case class Not(not: ChoiceRule) extends AbstractNot

  case class TopNot(not: ChoiceRule, next: String) extends AbstractNot with TopLevelChoice

}


case class ChoiceState(choices: List[TopLevelChoice],
                       default: Option[String],
                       inputPath: JsonPath = State.CONTEXT_ROOT,
                       outputPath: JsonPath = State.CONTEXT_ROOT)
  extends State("Choice") with Input with Output {

  override def decide(context: Context): Try[(String, Context)] = Try {
    val result = choices.find(rule => rule.evaluate(context)).map(rule => rule.next)
    if (result.isEmpty && default.isEmpty) {
      throw StateException.UnresolvableChoiceException(this, context)
    } else {
      (result.getOrElse(default.get), context)
    }
  }
}
