package com.akkastrator.state

import com.akkastrator.state.States.{Action, Computation, Decision, InputOutput, State, TransactionContext, Transition}
import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object PassState {
  implicit val passStateRead: Reads[PassState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "End").read[Boolean] and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Result").readNullable[JsonNode]
    ) (PassState.apply _)

}

case class PassState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                     resultPath: JsonPath = Step.CONTEXT_ROOT,
                     outputPath: JsonPath = Step.CONTEXT_ROOT,
                     end: Boolean = false,
                     next: Option[String] = None,
                     parameters: Option[JsonNode] = None,
                     comment: Option[String] = None,
                     result: Option[JsonNode])
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

