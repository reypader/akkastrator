package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.State
import com.akkastrator.state.conditions.LogicalConditions.AbstractEqual
import com.fasterxml.jackson.databind.node.BooleanNode
import com.jayway.jsonpath.JsonPath

object BooleanConditions {

  trait BooleanSupport extends Comparison[Boolean] with VariableAccess[Boolean] {
    def expectedValue: BooleanNode

    override def comparableValue: Boolean = expectedValue.booleanValue()

    override def getActualValue(context: State#Context, variable: JsonPath): Boolean = {
      context.read(variable).asInstanceOf[BooleanNode].booleanValue()
    }
  }

  abstract class AbstractBooleanEquals(variable: JsonPath, booleanEquals: BooleanNode) extends AbstractEqual[Boolean](variable) with BooleanSupport {
    override def expectedValue: BooleanNode = booleanEquals
  }

  case class BooleanEquals(variable: JsonPath, booleanEquals: BooleanNode) extends AbstractBooleanEquals(variable, booleanEquals)

  case class TopBooleanEquals(variable: JsonPath, booleanEquals: BooleanNode, next: String) extends AbstractBooleanEquals(variable, booleanEquals) with TopLevelChoice

}
