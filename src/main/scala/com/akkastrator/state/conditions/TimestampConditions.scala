package com.akkastrator.state.conditions

import java.time.OffsetDateTime

import com.akkastrator.state.ChoiceState.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.State
import com.akkastrator.state.common.State.State
import com.akkastrator.state.conditions.LogicalConditions.{AbstractEqual, AbstractGreaterThan, AbstractLessThan}
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath

object TimestampConditions {

  trait TimestampSupport extends Comparison[OffsetDateTime] with VariableAccess[OffsetDateTime] {
    def expectedValue: TextNode

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(expectedValue.textValue())

    override def getActualValue(context: State#Context, variable: JsonPath): OffsetDateTime = {
      OffsetDateTime.parse(context.read(variable).asInstanceOf[TextNode].textValue())
    }
  }

  abstract class AbstractTimestampEquals(variable: JsonPath, timestampEquals: TextNode) extends AbstractEqual[OffsetDateTime](variable) with TimestampSupport {
    override def expectedValue: TextNode = timestampEquals

    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.isEqual(actualValue)
    }
  }

  abstract class AbstractTimestampLessThan(variable: JsonPath, timestampLessThan: TextNode) extends AbstractLessThan[OffsetDateTime](variable) with TimestampSupport {
    override def expectedValue: TextNode = timestampLessThan
    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue)
    }
  }

  abstract class AbstractTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: TextNode) extends AbstractGreaterThan[OffsetDateTime](variable) with TimestampSupport {
    override def expectedValue: TextNode = timestampGreaterThan
    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue)
    }
  }

  case class TimestampEquals(variable: JsonPath, timestampEquals: TextNode) extends AbstractTimestampEquals(variable, timestampEquals)

  case class TimestampLessThan(variable: JsonPath, timestampLessThan: TextNode) extends AbstractTimestampLessThan(variable, timestampLessThan)

  case class TimestampGreaterThan(variable: JsonPath, timestampGreaterThan: TextNode) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan)


  case class TopTimestampEquals(variable: JsonPath, timestampEquals: TextNode, next: String) extends AbstractTimestampEquals(variable, timestampEquals) with TopLevelChoice

  case class TopTimestampLessThan(variable: JsonPath, timestampLessThan: TextNode, next: String) extends AbstractTimestampLessThan(variable, timestampLessThan) with TopLevelChoice

  case class TopTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: TextNode, next: String) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan) with TopLevelChoice

}
