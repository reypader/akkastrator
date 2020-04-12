package com.akkastrator.state

import com.akkastrator.state.common.States.{Action, Computation, Decision, InputOutput, State, TransactionContext, Transition, jsonPathRead}
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object PassState {
  implicit val passStateRead: Reads[PassState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "ResultPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "End").readWithDefault(false) and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Result").readNullable[JsonNode]
    ) (PassState.apply _)

}

case class PassState(inputPath: Option[JsonPath] = None,
                     resultPath: Option[JsonPath] = None,
                     outputPath: Option[JsonPath] = None,
                     end: Boolean,
                     next: Option[String] = None,
                     parameters: Option[JsonNode] = None,
                     comment: Option[String] = None,
                     result: Option[JsonNode] = None)
  extends State("Pass", comment) with InputOutput with Computation with Transition {
  if (end && next.isDefined) {
    throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
  }
  if (!end && next.isEmpty) {
    throw new IllegalArgumentException("`next` step must be defined if `end` is false")
  }

  override def prepare(context: TransactionContext): Try[Action] = Try {
    val effectiveInput = result.getOrElse(getInput(context))
    Action(s"internal:execution:${context.transactionId}", effectiveInput)
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    val newContext = writeResult(context, data)
    Decision(getOutput(newContext) copy (currentState = getNext))
  }
}

