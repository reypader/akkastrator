package com.akkastrator.state

import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Action, Decision, InputOutput, State, TransactionContext}
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try
import com.akkastrator.state.common.States.jsonPathRead

object SucceedState {
  implicit val succeedStateRead: Reads[SucceedState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "Comment").readNullable[String]
    ) (SucceedState.apply _)
}

case class SucceedState(inputPath: Option[JsonPath] = None,
                        outputPath: Option[JsonPath] = None,
                        comment: Option[String] = None)
  extends State("Succeed", comment) with InputOutput {
  override def prepare(context: TransactionContext): Try[Action] = Try {
    Action("internal:execution:${context.transactionId}", getInput(context))
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    Decision(getOutput(context) copy (currentState = States.END))
  }
}
