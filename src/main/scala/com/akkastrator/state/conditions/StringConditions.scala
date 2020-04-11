package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, VariableAccess}
import com.akkastrator.state.common.States
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, Reads}
import com.akkastrator.state.common.States.jsonPathRead

object StringConditions {

  trait StringSupport extends Comparison[String] with VariableAccess[String] {
    override def getActualValue(context: States.Context, variable: JsonPath): String = {
      context.read[TextNode](variable).textValue()
    }
  }

  abstract class AbstractStringEquals(variable: JsonPath, stringEquals: String) extends AbstractEqual[String](variable) with StringSupport {
    override def comparableValue: String = stringEquals
  }

  abstract class AbstractStringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractLessThan[String](variable) with StringSupport {
    override def comparableValue: String = stringLessThan
  }

  abstract class AbstractStringLessThanEquals(variable: JsonPath, stringLessThanEquals: String) extends AbstractLessThanEquals[String](variable) with StringSupport {
    override def comparableValue: String = stringLessThanEquals
  }

  abstract class AbstractStringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractGreaterThan[String](variable) with StringSupport {
    override def comparableValue: String = stringGreaterThan
  }

  abstract class AbstractStringGreaterThanEquals(variable: JsonPath, stringGreaterThanEquals: String) extends AbstractGreaterThanEquals[String](variable) with StringSupport {
    override def comparableValue: String = stringGreaterThanEquals
  }

  case class StringEquals(variable: JsonPath, stringEquals: String) extends AbstractStringEquals(variable, stringEquals)

  case class StringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractStringLessThan(variable, stringLessThan)

  case class StringLessThanEquals(variable: JsonPath, stringLessThanEquals: String) extends AbstractStringLessThanEquals(variable, stringLessThanEquals)

  case class StringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractStringGreaterThan(variable, stringGreaterThan)

  case class StringGreaterThanEquals(variable: JsonPath, stringGreaterThanEquals: String) extends AbstractStringGreaterThanEquals(variable, stringGreaterThanEquals)

  implicit val stringEqualsReads: Reads[StringEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "StringEquals").read[String]
    ) (StringEquals.apply _)

  implicit val stringLessThanReads: Reads[StringLessThan] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "StringLessThan").read[String]
    ) (StringLessThan.apply _)

  implicit val stringLessThanEqualsReads: Reads[StringLessThanEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "StringLessThanEquals").read[String]
    ) (StringLessThanEquals.apply _)

  implicit val stringGreaterThanReads: Reads[StringGreaterThan] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "StringGreaterThan").read[String]
    ) (StringGreaterThan.apply _)

  implicit val stringGreaterThanEqualsReads: Reads[StringGreaterThanEquals] = (
    (JsPath \ "Variable").read[JsonPath] and
      (JsPath \ "StringGreaterThanEquals").read[String]
    ) (StringGreaterThanEquals.apply _)


}
