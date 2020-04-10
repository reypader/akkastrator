package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, VariableAccess}
import com.akkastrator.state.States
import com.akkastrator.state.conditions.LogicalConditions.AbstractEqual
import com.fasterxml.jackson.databind.node.BooleanNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, Reads}

object BooleanConditions {

  trait BooleanSupport extends Comparison[Boolean] with VariableAccess[Boolean] {
    override def getActualValue(context: States.Context, variable: JsonPath): Boolean = {
      context.read[BooleanNode](variable).booleanValue()
    }
  }

  abstract class AbstractBooleanEquals(variable: JsonPath, booleanEquals: Boolean) extends AbstractEqual[Boolean](variable) with BooleanSupport {
    override def comparableValue: Boolean = booleanEquals
  }

  case class BooleanEquals(variable: JsonPath, booleanEquals: Boolean) extends AbstractBooleanEquals(variable, booleanEquals)

  implicit val booleanEqualsReads: Reads[BooleanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "BooleanEquals").read[Boolean]
    ) (BooleanEquals.apply _)
}
