package com.akkastrator.state

import com.akkastrator.state.States.{Action, Computation, Decision, ErrorHandling, InputOutput, State, TransactionContext, Transition}
import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object ParallelState {
  implicit val stateMachineListRead: Reads[List[StateMachine]] = Reads.list[StateMachine]

  implicit val parallelStateRead: Reads[ParallelState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "End").read[Boolean] and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Retry").readNullable[ErrorRetry] and
      (JsPath \ "Catch").readNullable[ErrorCatch] and
      (JsPath \ "Branches").read[List[StateMachine]]
    ) (ParallelState.apply _)
}

case class ParallelState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                         resultPath: JsonPath = Step.CONTEXT_ROOT,
                         outputPath: JsonPath = Step.CONTEXT_ROOT,
                         end: Boolean = false,
                         next: Option[String] = None,
                         parameters: Option[JsonNode] = None,
                         comment: Option[String] = None,
                         errorRetry: Option[ErrorRetry] = None,
                         errorCatch: Option[ErrorCatch] = None,
                         branches: List[StateMachine])
  extends State("Parallel", comment) with InputOutput with Computation with Transition with ErrorHandling {
  if (branches == null | branches.isEmpty) {
    throw new IllegalArgumentException("branches must be specified")
  }

  override def prepare(context: TransactionContext): Try[Action] = Try {
    val effectiveInput = getInput(context).deepCopy[JsonNode]()
    Action(s"internal:parallel:${context.transactionId}:${context.currentState}", effectiveInput)
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    val newContext = writeResult(context, data) //TODO: error/retry
    Decision(getOutput(newContext) copy (currentState = getNext))
  }
}