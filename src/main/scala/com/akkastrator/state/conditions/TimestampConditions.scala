package com.akkastrator.state.conditions

import java.time.OffsetDateTime

import com.akkastrator.state.ChoiceState.{Comparison, VariableAccess}
import com.akkastrator.state.common.States
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}
import com.akkastrator.state.common.States.jsonPathRead

object TimestampConditions {

  trait TimestampSupport extends Comparison[OffsetDateTime] with VariableAccess[OffsetDateTime] {
    override def getActualValue(context: States.Context, variable: JsonPath): OffsetDateTime = {
      OffsetDateTime.parse(context.read[TextNode](variable).textValue())
    }
  }

  abstract class AbstractTimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractEqual[OffsetDateTime](variable) with TimestampSupport {
    override def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.isEqual(actualValue)
    }

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampEquals)
  }

  abstract class AbstractTimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractLessThan[OffsetDateTime](variable) with TimestampSupport {
    override def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue)
    }

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampLessThan)
  }

  abstract class AbstractTimestampLessThanEquals(variable: JsonPath, timestampLessThanEquals: String) extends AbstractLessThanEquals[OffsetDateTime](variable) with TimestampSupport {
    override def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isBefore(comparableValue) || actualValue.isEqual(comparableValue)
    }

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampLessThanEquals)
  }

  abstract class AbstractTimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractGreaterThan[OffsetDateTime](variable) with TimestampSupport {
    override def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue)
    }

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampGreaterThan)
  }

  abstract class AbstractTimestampGreaterThanEquals(variable: JsonPath, timestampGreaterThanEquals: String) extends AbstractGreaterThanEquals[OffsetDateTime](variable) with TimestampSupport {
    override def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.isAfter(comparableValue) || actualValue.isEqual(comparableValue)
    }

    override def comparableValue: OffsetDateTime = OffsetDateTime.parse(timestampGreaterThanEquals)
  }

  case class TimestampEquals(variable: JsonPath, timestampEquals: String) extends AbstractTimestampEquals(variable, timestampEquals)

  case class TimestampLessThan(variable: JsonPath, timestampLessThan: String) extends AbstractTimestampLessThan(variable, timestampLessThan)

  case class TimestampLessThanEquals(variable: JsonPath, timestampLessThanEquals: String) extends AbstractTimestampLessThanEquals(variable, timestampLessThanEquals)

  case class TimestampGreaterThan(variable: JsonPath, timestampGreaterThan: String) extends AbstractTimestampGreaterThan(variable, timestampGreaterThan)

  case class TimestampGreaterThanEquals(variable: JsonPath, timestampGreaterThanEquals: String) extends AbstractTimestampGreaterThanEquals(variable, timestampGreaterThanEquals)

  implicit val timestampEqualsReads: Reads[TimestampEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "TimestampEquals").read[String]
    ) (TimestampEquals.apply _)

  implicit val timestampLessThanReads: Reads[TimestampLessThan] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "TimestampLessThan").read[String]
    ) (TimestampLessThan.apply _)

  implicit val timestampLessThanEqualsReads: Reads[TimestampLessThanEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "TimestampLessThanEquals").read[String]
    ) (TimestampLessThanEquals.apply _)

  implicit val timestampGreaterThanReads: Reads[TimestampGreaterThan] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "TimestampGreaterThan").read[String]
    ) (TimestampGreaterThan.apply _)

  implicit val timestampGreaterThanEqualsReads: Reads[TimestampGreaterThanEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "TimestampGreaterThanEquals").read[String]
    ) (TimestampGreaterThanEquals.apply _)
}
