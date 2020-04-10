package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, VariableAccess}
import com.akkastrator.state.States
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.NumericNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

object NumericConditions {

  trait NumericSupport extends Comparison[BigDecimal] with VariableAccess[BigDecimal] {
    override def getActualValue(context: States.Context, variable: JsonPath): BigDecimal = {
      context.read[NumericNode](variable).decimalValue()
    }
  }

  abstract class AbstractNumericEquals(variable: JsonPath, numericEquals: BigDecimal) extends AbstractEqual[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericEquals
  }

  abstract class AbstractNumericLessThan(variable: JsonPath, numericLessThan: BigDecimal) extends AbstractLessThan[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericLessThan
  }

  abstract class AbstractNumericLessThanEquals(variable: JsonPath, numericLessThanEquals: BigDecimal) extends AbstractLessThanEquals[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericLessThanEquals
  }

  abstract class AbstractNumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractGreaterThan[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericGreaterThan
  }

  abstract class AbstractNumericGreaterThanEquals(variable: JsonPath, numericGreaterThanEquals: BigDecimal) extends AbstractGreaterThanEquals[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericGreaterThanEquals
  }

  case class NumericEquals(variable: JsonPath, numericEquals: BigDecimal) extends AbstractNumericEquals(variable, numericEquals)

  case class NumericLessThan(variable: JsonPath, numericLessThan: BigDecimal) extends AbstractNumericLessThan(variable, numericLessThan)

  case class NumericLessThanEquals(variable: JsonPath, numericLessThanEquals: BigDecimal) extends AbstractNumericLessThanEquals(variable, numericLessThanEquals)

  case class NumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractNumericGreaterThan(variable, numericGreaterThan)

  case class NumericGreaterThanEquals(variable: JsonPath, numericGreaterThanEquals: BigDecimal) extends AbstractNumericGreaterThanEquals(variable, numericGreaterThanEquals)

  implicit val numericEqualsReads: Reads[NumericEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "NumericEquals").read[BigDecimal]
    ) (NumericEquals.apply _)

  implicit val numericLessThanReads: Reads[NumericLessThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "NumericLessThan").read[BigDecimal]
    ) (NumericLessThan.apply _)

  implicit val numericLessThanEqualsReads: Reads[NumericLessThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "NumericLessThanEquals").read[BigDecimal]
    ) (NumericLessThanEquals.apply _)

  implicit val numericGreaterThanReads: Reads[NumericGreaterThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "NumericGreaterThan").read[BigDecimal]
    ) (NumericGreaterThan.apply _)

  implicit val numericGreaterThanEqualsReads: Reads[NumericGreaterThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "NumericGreaterThanEquals").read[BigDecimal]
    ) (NumericGreaterThanEquals.apply _)
}
