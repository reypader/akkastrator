package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.akkastrator.state.StateException.ErrorDetails
import com.akkastrator.state.States.{Decision, TransactionContext}
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

  def errorCatch: List[Catcher]

  final def handle(context: TransactionContext, error: ErrorDetails): Try[Decision] = Try {
    val catcherResult: Option[Catcher] = errorCatch.find(catcher => {
      (catcher.errorEquals.length == 1 && catcher.errorEquals.contains(CatchError.ALL)) || catcher.errorEquals.contains(error.error)
    })
    catcherResult match {
      case Some(catcher) => doHandle(context, error, catcher)
      case None => throw StateException.StateFailure("States.NoCatcherMatched", error.error)
    }
  }

  def doHandle(context: TransactionContext, error: ErrorDetails, catcher: Catcher): Decision = {
    val errorOut = CatchError.makeNode(error.error, error.cause)
    val out = if (catcher.resultPath == Step.CONTEXT_ROOT) {
      context.copy(data = Step.PARSER.parse(errorOut), currentState = catcher.next)
    } else {
      catcher.writeResult(context, errorOut) copy (currentState = catcher.next)

    }
    Decision(out)
  }

  case class Catcher(errorEquals: List[String], next: String, resultPath: JsonPath = Step.CONTEXT_ROOT) extends Result

}
