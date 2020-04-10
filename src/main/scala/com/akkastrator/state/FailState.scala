package com.akkastrator.state

import com.akkastrator.state.States.{Action, Decision, State, TransactionContext}
import com.akkastrator.state.common.{CatchError, Step}
import com.fasterxml.jackson.databind.JsonNode
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object FailState {
  implicit val failStateRead: Reads[FailState] = (
    (JsPath \ "Error").read[String] and
      (JsPath \ "Cause").read[String] and
      (JsPath \ "Comment").readNullable[String]
    ) (FailState.apply _)
}

case class FailState(error: String, cause: String, comment: Option[String] = None)
  extends State("Fail", comment) {
  override def prepare(context: TransactionContext): Try[Action] = Try {
    Action(s"internal:execution:${context.transactionId}", CatchError.makeNode(error, cause))
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    Decision(context.copy(data = Step.PARSER.parse(data), currentState = States.END))
  }
}