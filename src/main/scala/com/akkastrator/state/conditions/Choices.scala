package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.BooleanConditions.BooleanEquals
import com.akkastrator.state.conditions.LogicalConditions.{And, Not, Or}
import com.akkastrator.state.conditions.NumericConditions._
import com.akkastrator.state.conditions.StringConditions._
import com.akkastrator.state.conditions.TimestampConditions._
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object Choices {
  implicit val booleanEqualsReads: Reads[BooleanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "BooleanEquals").read[Boolean]
    ) (BooleanEquals.apply _)


  implicit val stringEqualsReads: Reads[StringEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "StringEquals").read[String]
    ) (StringEquals.apply _)

  implicit val stringLessThanReads: Reads[StringLessThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "StringLessThan").read[String]
    ) (StringLessThan.apply _)

  implicit val stringLessThanEqualsReads: Reads[StringLessThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "StringLessThanEquals").read[String]
    ) (StringLessThanEquals.apply _)

  implicit val stringGreaterThanReads: Reads[StringGreaterThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "StringGreaterThan").read[String]
    ) (StringGreaterThan.apply _)

  implicit val stringGreaterThanEqualsReads: Reads[StringGreaterThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "StringGreaterThanEquals").read[String]
    ) (StringGreaterThanEquals.apply _)


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
  implicit val timestampEqualsReads: Reads[TimestampEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "TimestampEquals").read[String]
    ) (TimestampEquals.apply _)

  implicit val timestampLessThanReads: Reads[TimestampLessThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "TimestampLessThan").read[String]
    ) (TimestampLessThan.apply _)

  implicit val timestampLessThanEqualsReads: Reads[TimestampLessThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "TimestampLessThanEquals").read[String]
    ) (TimestampLessThanEquals.apply _)

  implicit val timestampGreaterThanReads: Reads[TimestampGreaterThan] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "TimestampGreaterThan").read[String]
    ) (TimestampGreaterThan.apply _)

  implicit val timestampGreaterThanEqualsReads: Reads[TimestampGreaterThanEquals] = (
    (JsPath \ "Variable").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "TimestampGreaterThanEquals").read[String]
    ) (TimestampGreaterThanEquals.apply _)
  implicit val choiceRuleRead: Reads[ChoiceRule] = (json: JsValue) => Try {
    json match {
      case JsObject(o) if o.contains("StringEquals") => stringEqualsReads
      case JsObject(o) if o.contains("StringLessThan") => stringLessThanReads
      case JsObject(o) if o.contains("StringGreaterThan") => stringGreaterThanReads
      case JsObject(o) if o.contains("StringLessThanEquals") => stringLessThanEqualsReads
      case JsObject(o) if o.contains("StringGreaterThanEquals") => stringGreaterThanEqualsReads
      case JsObject(o) if o.contains("NumericEquals") => numericEqualsReads
      case JsObject(o) if o.contains("NumericLessThan") => numericLessThanReads
      case JsObject(o) if o.contains("NumericGreaterThan") => numericGreaterThanReads
      case JsObject(o) if o.contains("NumericLessThanEquals") => numericLessThanEqualsReads
      case JsObject(o) if o.contains("NumericGreaterThanEquals") => numericGreaterThanEqualsReads
      case JsObject(o) if o.contains("BooleanEquals") => booleanEqualsReads
      case JsObject(o) if o.contains("TimestampEquals") => timestampEqualsReads
      case JsObject(o) if o.contains("TimestampLessThan") => timestampLessThanReads
      case JsObject(o) if o.contains("TimestampGreaterThan") => timestampGreaterThanReads
      case JsObject(o) if o.contains("TimestampLessThanEquals") => timestampLessThanEqualsReads
      case JsObject(o) if o.contains("TimestampGreaterThanEquals") => timestampGreaterThanEqualsReads
      case JsObject(o) if o.contains("And") => andRead
      case JsObject(o) if o.contains("Or") => orRead
      case JsObject(o) if o.contains("Not") => notRead
      case _ => throw new RuntimeException(s"Unidentifiable Choice : ${json.toString()}")
    }
  }.map(_.reads(json)).recover(ex => JsError(ex.getMessage)).get


  implicit val topLevelChoiceRead: Reads[TopLevelChoice] = (json: JsValue) => {
    choiceRuleRead.reads(json).flatMap(x => json match {
      case JsObject(o) if o.contains("Next") => JsSuccess(new TopLevelChoice {
        override def evaluate(context: Step#Context): Boolean = x.evaluate(context)

        override def next: String = o.get("Next").asInstanceOf[JsString].value
      })
      case _ => JsError(s"Top level choice must have 'Next' : ${json.toString()}")
    })
  }
  implicit val choiceRuleListRead: Reads[List[ChoiceRule]] = Reads.list[ChoiceRule]
  implicit val andRead: Reads[And] = Json.reads[And]
  implicit val orRead: Reads[Or] = Json.reads[Or]
  implicit val notRead: Reads[Not] = Json.reads[Not]

  trait ChoiceRule {
    def evaluate(context: Step#Context): Boolean
  }

  trait Comparison[T] {
    def comparableValue: T
  }

  trait VariableAccess[T] {
    def getActualValue(context: Step#Context, variable: JsonPath): T
  }

  trait TopLevelChoice extends ChoiceRule {
    def next: String
  }

}
