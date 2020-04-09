package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.akkastrator.state.StateException.ErrorDetails
import com.akkastrator.state.common.State.State
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath

import scala.util.Try

object CatchError {
  val ALL: String = "States.ALL"

  val om = new ObjectMapper()

  def makeNode(error: String, cause: String): JsonNode = {
    om.createObjectNode().put("error", error).put("cause", cause)
  }
}

trait CatchError {

  case class Catcher(errorEquals: List[String], next: String, resultPath: JsonPath = State.CONTEXT_ROOT) extends Result

  def catchError: List[Catcher]

  def doHandle(context: State#Context, error: ErrorDetails, catcher: Catcher): (String, State#Context)= {
    val errorOut = CatchError.makeNode(error.error, error.cause)
    val out = if (catcher.resultPath == State.CONTEXT_ROOT) {
      State.PARSER.parse(errorOut)
    } else {
      catcher.writeResult(context, errorOut)
    }
    (catcher.next, out)
  }

  final def handle(context: State#Context, error: ErrorDetails): Try[(String, State#Context)] = Try{
    val catcherResult: Option[Catcher] = catchError.find(catcher => {
      (catcher.errorEquals.length == 1 && catcher.errorEquals.contains(CatchError.ALL)) || catcher.errorEquals.contains(error.error)
    })
    catcherResult match {
      case Some(catcher) => doHandle(context, error, catcher)
      case None => throw StateException.StateFailure("States.NoCatcherMatched", error.error)
    }
  }

}
