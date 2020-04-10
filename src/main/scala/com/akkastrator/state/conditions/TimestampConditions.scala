package com.akkastrator.state.conditions

import java.time.OffsetDateTime

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices._
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath

object TimestampConditions {

  trait TimestampSupport extends Comparison[OffsetDateTime] with VariableAccess[OffsetDateTime] {
    override def getActualValue(context: Step#Context, variable: JsonPath): OffsetDateTime = {
      OffsetDateTime.parse(context.read(variable).asInstanceOf[TextNode].textValue())
    }
  }

  abstract class AbstractTimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractEqual[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampEquals)

    override def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.isEqual(actualValue)
    }
  }

  abstract class AbstractTimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractLessThan[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampLessThan)

    override def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue)
    }
  }

  abstract class AbstractTimestampLessThanEquals(variable: JsonPath, timestampLessThanEquals: String) extends AbstractLessThanEquals[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampLessThanEquals)

    override def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue) || actualValue.isEqual(comparableValue)
    }
  }

  abstract class AbstractTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractGreaterThan[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampGreaterThan)

    override def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue)
    }
  }

  abstract class AbstractTimestampGreaterThanEquals(variable: JsonPath, timestampGreaterThanEquals: String) extends AbstractGreaterThanEquals[OffsetDateTime](variable) with TimestampSupport {
    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampGreaterThanEquals)

    override def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue) || actualValue.isEqual(comparableValue)
    }
  }

  case class TimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractTimestampEquals(variable, timestampEquals)

  case class TimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractTimestampLessThan(variable, timestampLessThan)

  case class TimestampLessThanEquals(variable: JsonPath, timestampLessThanEquals: String) extends AbstractTimestampLessThanEquals(variable, timestampLessThanEquals)

  case class TimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan)

  case class TimestampGreaterThanEquals(variable: JsonPath, timestampGreaterThanEquals: String) extends AbstractTimestampGreaterThanEquals(variable, timestampGreaterThanEquals)

}
