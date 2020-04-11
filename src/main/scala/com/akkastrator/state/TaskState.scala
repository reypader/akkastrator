package com.akkastrator.state

import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Action, Computation, Decision, ErrorHandling, InputOutput, State, TransactionContext, Transition}
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try
import com.akkastrator.state.common.States.jsonPathRead

object TaskState {
  implicit val taskStateRead: Reads[TaskState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "ResultPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "End").readWithDefault(false) and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Retry").readNullable[ErrorRetry] and
      (JsPath \ "Catch").readNullable[ErrorCatch] and
      (JsPath \ "Resource").read[String] and
      (JsPath \ "TimeoutSeconds").read[Int] and
      (JsPath \ "HeartBeatSeconds").readNullable[Int]
    ) (TaskState.apply _)
}

case class TaskState(inputPath: Option[JsonPath] = None,
                     resultPath: Option[JsonPath] = None,
                     outputPath: Option[JsonPath] = None,
                     end: Boolean = false,
                     next: Option[String] = None,
                     parameters: Option[JsonNode] = None,
                     comment: Option[String] = None,
                     errorRetry: Option[ErrorRetry] = None,
                     errorCatch: Option[ErrorCatch] = None,
                     resource: String,
                     timeoutSeconds: Int = 60,
                     heartBeatSeconds: Option[Int])
  extends State("Task", comment) with InputOutput with Computation with Transition with ErrorHandling {
  if (resource == null || resource.isBlank) {
    throw new IllegalArgumentException("resource must not be blank")
  }
  if (timeoutSeconds < 1) {
    throw new IllegalArgumentException("timeoutSeconds must be positive")
  }
  if (heartBeatSeconds.isDefined && heartBeatSeconds.get < 1) {
    throw new IllegalArgumentException("heartBeatSeconds must be positive")
  }
  if (heartBeatSeconds.isDefined && heartBeatSeconds.get > timeoutSeconds) {
    throw new IllegalArgumentException("heartBeatSeconds must be less than timeoutSeconds")
  }
  if (end && next.isDefined) {
    throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
  }
  if (!end && next.isEmpty) {
    throw new IllegalArgumentException("`next` step must be defined if `end` is false")
  }

  override def prepare(context: TransactionContext): Try[Action] = Try {
    val effectiveInput = getInput(context)
    Action(resource, effectiveInput)
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    val newContext = writeResult(context, data) //TODO: error/retry/timeout
    Decision(getOutput(newContext) copy (currentState = getNext))
  }
}
