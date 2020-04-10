package com.akkastrator.state

import com.akkastrator.state.common._
import com.akkastrator.state.conditions.Choices._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try

object States {

  val om = new ObjectMapper()

  implicit val jsonNodeRead: Reads[JsonNode] = (m: JsValue) => Try {
    om.readTree(m.toString())
  }.map(JsSuccess(_)).recover(ex => JsError(ex.getMessage)).get

  private def typeOf(o: scala.collection.Map[String, JsValue], typeString: String): Boolean = {
    o.get("Type").asInstanceOf[JsString].value == typeString
  }

  implicit val choiceRuleRead: Reads[State] = (json: JsValue) => Try {
    json match {
      case JsObject(o) if typeOf(o, "Pass") => passStateRead
      case JsObject(o) if typeOf(o, "Task") => taskStateRead
      case JsObject(o) if typeOf(o, "Choice") => choiceStateRead
      case JsObject(o) if typeOf(o, "Wait") => waitStateRead
      case JsObject(o) if typeOf(o, "Succeed") => succeedStateRead
      case JsObject(o) if typeOf(o, "Fail") => failStateRead
      case JsObject(o) if typeOf(o, "Parallel") => parallelStateRead
      case _ => throw new IllegalArgumentException("Unrecognized state type")
    }
  }.map(_.reads(json)).recover(ex => JsError(ex.getMessage)).get


  implicit val stateMapRead: Reads[Map[String, State]] = Reads.map[State]

  implicit val stateMachineRead: Reads[StateMachine] = (
    (JsPath \ "StartAt").read[String] and
      (JsPath \ "States").read[Map[String, State]] and
      (JsPath \ "TimeoutSeconds").readNullable[Int] and
      (JsPath \ "Version").read[String] and
      (JsPath \ "Comment").readNullable[String]
    ) (StateMachine.apply _)

  implicit val stringListRead: Reads[List[String]] = Reads.list[String]
  implicit val topLevelChoiceListRead: Reads[List[TopLevelChoice]] = Reads.list[TopLevelChoice]
  implicit val stateMachineListRead: Reads[List[StateMachine]] = Reads.list[StateMachine]

  implicit val errorRetryRead: Reads[ErrorRetry] = (
    (JsPath \ "ErrorEquals").read[List[String]] and
      (JsPath \ "IntervalSeconds").read[Int] and
      (JsPath \ "MaxAttempts").read[Int] and
      (JsPath \ "BackOffRate").read[BigDecimal] and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s))
    ) (ErrorRetry.apply _)

  implicit val errorCatchRead: Reads[ErrorCatch] = (
    (JsPath \ "ErrorEquals").read[List[String]] and
      (JsPath \ "Next").read[String] and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s))
    ) (ErrorCatch.apply _)

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
  implicit val taskStateRead: Reads[TaskState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "End").read[Boolean] and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Retry").readNullable[ErrorRetry] and
      (JsPath \ "Catch").readNullable[ErrorCatch] and
      (JsPath \ "Resource").read[String] and
      (JsPath \ "TimeoutSeconds").read[Int] and
      (JsPath \ "HeartBeatSeconds").readNullable[Int]
    ) (TaskState.apply _)
  implicit val choiceStateRead: Reads[ChoiceState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Choices").read[List[TopLevelChoice]] and
      (JsPath \ "Default").readNullable[String]
    ) (ChoiceState.apply _)

  implicit val waitStateRead: Reads[WaitState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "End").read[Boolean] and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Seconds").readNullable[Int] and
      (JsPath \ "Timestamp").readNullable[String] and
      (JsPath \ "SecondsPath").readNullable[String].map(s => s.map(JsonPath.compile(_))) and
      (JsPath \ "TimestampPath").readNullable[String].map(s => s.map(JsonPath.compile(_)))
    ) (WaitState.apply _)
  implicit val succeedStateRead: Reads[SucceedState] = (
    (JsPath \ "InputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "OutputPath").read[String].map(s => JsonPath.compile(s)) and
      (JsPath \ "Comment").readNullable[String]
    ) (SucceedState.apply _)
  implicit val failStateRead: Reads[FailState] = (JsPath \ "Comment").readNullable[String].map(FailState.apply)
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

  trait InputOutput extends Input with Output

  trait Computation extends Parameter with Result

  trait Transition {
    def next: Option[String]

    def end: Boolean
  }

  trait ErrorHandling {
    def errorRetry: Option[ErrorRetry]

    def errorCatch: Option[ErrorCatch]
  }

  abstract class State(stateType: String, comment: Option[String])

  case class StateMachine(startAt: String,
                          states: Map[String, State],
                          timeoutSeconds: Option[Int],
                          version: String = "1.0",
                          comment: Option[String] = None) extends LazyLogging {
    states.foreach({
      case (stateName, state: PassState) =>
        if (state.next.isDefined && !states.contains(state.next.get)) {
          throw new IllegalArgumentException(s"next field for $stateName must be present in the same state machine")
        }
      case (stateName, state: TaskState) =>
        if (state.next.isDefined && !states.contains(state.next.get)) {
          throw new IllegalArgumentException(s"next field for $stateName must be present in the same state machine")
        }
      case (stateName, state: ChoiceState) =>
        if (state.default.isDefined && !states.contains(state.default.get)) {
          throw new IllegalArgumentException(s"default field for $stateName must be present in the same state machine")
        }
        state.choices.foreach { choice =>
          if (!states.contains(choice.next)) {
            throw new IllegalArgumentException(s"next field of choices for $stateName must be present in the same state machine")
          }
        }
      case (stateName, state: WaitState) =>
        if (state.next.isDefined && !states.contains(state.next.get)) {
          throw new IllegalArgumentException(s"next field for $stateName must be present in the same state machine")
        }
      case (stateName, state: ParallelState) =>
        if (state.next.isDefined && !states.contains(state.next.get)) {
          throw new IllegalArgumentException(s"next field for $stateName must be present in the same state machine")
        }
      case (stateName, _) => logger.warn(s"No validation done for $stateName")
    })
  }

  case class ErrorCatch(errorEquals: List[String],
                        next: String,
                        resultPath: JsonPath = Step.CONTEXT_ROOT) {
    if (errorEquals == null || errorEquals.isEmpty) {
      throw new IllegalArgumentException("errorEquals must be specified")
    }
    if (errorEquals.contains(CatchError.ALL) && errorEquals.length != 1) {
      throw new IllegalArgumentException(CatchError.ALL + " must be the only element in errorEquals")
    }
    if (next == null || next.isBlank) {
      throw new IllegalArgumentException("next must not be blank")
    }
    if (resultPath == null) {
      throw new IllegalArgumentException("resultPath must not be null")
    }
  }

  case class ErrorRetry(errorEquals: List[String],
                        intervalSeconds: Int = 1,
                        maxAttempts: Int = 3,
                        backOffRate: BigDecimal = 2,
                        resultPath: JsonPath = Step.CONTEXT_ROOT) {
    if (errorEquals == null || errorEquals.isEmpty) {
      throw new IllegalArgumentException("errorEquals must be specified")
    }
    if (errorEquals.contains(CatchError.ALL) && errorEquals.length != 1) {
      throw new IllegalArgumentException(CatchError.ALL + " must be the only element in errorEquals")
    }
    if (intervalSeconds < 1) {
      throw new IllegalArgumentException("intervalSeconds must be positive non-zero")
    }
    if (maxAttempts < 0) {
      throw new IllegalArgumentException("intervalSeconds must be non-negative")
    }
    if (backOffRate < 1) {
      throw new IllegalArgumentException("backOffRate must be greated than or equal to 1")
    }
    if (resultPath == null) {
      throw new IllegalArgumentException("resultPath must not be null")
    }
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
  }

  case class TaskState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                       resultPath: JsonPath = Step.CONTEXT_ROOT,
                       outputPath: JsonPath = Step.CONTEXT_ROOT,
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
  }

  case class ChoiceState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                         outputPath: JsonPath = Step.CONTEXT_ROOT,
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
  }

  case class WaitState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                       outputPath: JsonPath = Step.CONTEXT_ROOT,
                       end: Boolean = false,
                       next: Option[String] = None,
                       comment: Option[String] = None,
                       seconds: Option[Int],
                       timestamp: Option[String],
                       secondsPath: Option[JsonPath],
                       timestampPath: Option[JsonPath])
    extends State("Wait", comment) with InputOutput with Transition {
    if (end && next.isDefined) {
      throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
    }
    if (!end && next.isEmpty) {
      throw new IllegalArgumentException("`next` step must be defined if `end` is false")
    }
    if (List(seconds.isDefined, timestamp.isDefined, secondsPath.isDefined, timestampPath.isDefined).count(b => b) != 1) {
      throw new IllegalArgumentException("There must be exactly one provided among seconds, timestamp, secondsPath, and timestampPath")
    }
  }

  case class SucceedState(inputPath: JsonPath = Step.CONTEXT_ROOT,
                          outputPath: JsonPath = Step.CONTEXT_ROOT,
                          comment: Option[String] = None)
    extends State("Succeed", comment) with InputOutput

  case class FailState(comment: Option[String] = None)
    extends State("Fail", comment)

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
  }

}
