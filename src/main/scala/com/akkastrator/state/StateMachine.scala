package com.akkastrator.state

import com.akkastrator.state.ChoiceState.choiceStateRead
import com.akkastrator.state.FailState.failStateRead
import com.akkastrator.state.ParallelState.parallelStateRead
import com.akkastrator.state.PassState.passStateRead
import com.akkastrator.state.States.State
import com.akkastrator.state.SucceedState.succeedStateRead
import com.akkastrator.state.TaskState.taskStateRead
import com.akkastrator.state.WaitState.waitStateRead
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.util.Try

object StateMachine {
  private def typeOf(o: scala.collection.Map[String, JsValue], typeString: String): Boolean = {
    o("Type").asInstanceOf[JsString].value == typeString
  }

  implicit val stateRead: Reads[State] = (json: JsValue) => Try {
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


}

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