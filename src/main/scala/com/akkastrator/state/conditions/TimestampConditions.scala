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
    override def getActualValue(context: State#Context, variable: JsonPath): OffsetDateTime = {
      OffsetDateTime.parse(context.read(variable).asInstanceOf[TextNode].textValue())
    }
  }

  abstract class AbstractTimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractEqual[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampEquals)
    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.isEqual(actualValue)
    }
  }

  abstract class AbstractTimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractLessThan[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampLessThan)
    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue)
    }
  }

  abstract class AbstractTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractGreaterThan[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampGreaterThan)
    override def evaluate(context: State#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue)
    }
  }

  case class TimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractTimestampEquals(variable, timestampEquals)

  case class TimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractTimestampLessThan(variable, timestampLessThan)

  case class TimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan)


  case class TopTimestampEquals(variable: JsonPath, timestampEquals: String, next: String) extends AbstractTimestampEquals(variable, timestampEquals) with TopLevelChoice

  case class TopTimestampLessThan(variable: JsonPath, timestampLessThan: String, next: String) extends AbstractTimestampLessThan(variable, timestampLessThan) with TopLevelChoice

  case class TopTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String, next: String) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan) with TopLevelChoice

}
