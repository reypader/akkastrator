package com.akkastrator.state

import com.akkastrator.state.ChoiceState.TopLevelChoice
import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Action, Decision, InputOutput, State, TransactionContext, jsonPathRead}
import com.akkastrator.state.conditions.BooleanConditions.booleanEqualsReads
import com.akkastrator.state.conditions.LogicalConditions.{andRead, notRead, orRead}
import com.akkastrator.state.conditions.NumericConditions.{numericEqualsReads, numericGreaterThanEqualsReads, numericGreaterThanReads, numericLessThanEqualsReads, numericLessThanReads}
import com.akkastrator.state.conditions.StringConditions.{stringEqualsReads, stringGreaterThanEqualsReads, stringGreaterThanReads, stringLessThanEqualsReads, stringLessThanReads}
import com.akkastrator.state.conditions.TimestampConditions.{timestampEqualsReads, timestampGreaterThanEqualsReads, timestampGreaterThanReads, timestampLessThanEqualsReads, timestampLessThanReads}
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object ChoiceState {
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
        override def inner : ChoiceRule = x
        override def next: String = o("Next").asInstanceOf[JsString].value
      })
      case _ => JsError(s"Top level choice must have 'Next' : ${json.toString()}")
    })
  }

  implicit val choiceRuleListRead: Reads[List[ChoiceRule]] = Reads.list[ChoiceRule]
  implicit val topLevelChoiceListRead: Reads[List[TopLevelChoice]] = Reads.list[TopLevelChoice]

  implicit val choiceStateRead: Reads[ChoiceState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Choices").read[List[TopLevelChoice]] and
      (JsPath \ "Default").readNullable[String]
    ) (ChoiceState.apply _)


  trait ChoiceRule {
    def evaluate(context: States.Context): Boolean
  }

  trait Comparison[T] {
    def comparableValue: T
  }

  trait VariableAccess[T] {
    def getActualValue(context: States.Context, variable: JsonPath): T
  }

  trait TopLevelChoice extends ChoiceRule {
    def inner : ChoiceRule
    def next: String
    override def evaluate(context: States.Context): Boolean = inner.evaluate(context)
  }

}


case class ChoiceState(inputPath: Option[JsonPath] = None,
                       outputPath: Option[JsonPath] = None,
                       comment: Option[String] = None,
                       choices: List[TopLevelChoice],
                       default: Option[String])
  extends State("Choice", comment) with InputOutput {
  if (choices == null || choices.isEmpty) {
    throw new IllegalArgumentException("choices must be specified")
  }

  if (choices.exists(c => c.next == null || c.next.isBlank)) {
    throw new IllegalArgumentException("All top level choices must have a 'next' field")
  }


  override def prepare(context: TransactionContext): Try[Action] = Try {
    val effectiveInput = getInput(context)
    Action(s"internal:execution:${context.transactionId}", effectiveInput)
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    val result = choices.find(rule => rule.evaluate(States.PARSER.parse(data))).map(rule => rule.next)
    if (result.isEmpty && default.isEmpty) {
      throw StateException.StateFailure("States.NoChoiceMatched", context.currentState)
    } else {
      Decision(getOutput(context) copy (currentState = result.getOrElse(default.get)))
    }
  }
}

