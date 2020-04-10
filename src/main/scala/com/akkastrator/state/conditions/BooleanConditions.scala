package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices._
import com.akkastrator.state.conditions.LogicalConditions.AbstractEqual
import com.fasterxml.jackson.databind.node.BooleanNode
import com.jayway.jsonpath.JsonPath

object BooleanConditions {

  trait BooleanSupport extends Comparison[Boolean] with VariableAccess[Boolean] {
    override def getActualValue(context: Step#Context, variable: JsonPath): Boolean = {
      context.read(variable).asInstanceOf[BooleanNode].booleanValue()
    }
  }

  abstract class AbstractBooleanEquals(variable: JsonPath, booleanEquals: Boolean) extends AbstractEqual[Boolean](variable) with BooleanSupport {
    override def comparableValue: Boolean = booleanEquals
  }

  case class BooleanEquals(variable: JsonPath, booleanEquals: Boolean) extends AbstractBooleanEquals(variable, booleanEquals)

  case class TopBooleanEquals(variable: JsonPath, booleanEquals: Boolean, next: String) extends AbstractBooleanEquals(variable, booleanEquals) with TopLevelChoice

}
