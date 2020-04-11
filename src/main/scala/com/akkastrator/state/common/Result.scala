package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.akkastrator.state.common.States._
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.{JsonPath, PathNotFoundException}

import scala.util.{Success, Try}

trait Result {
  def resultPath: Option[JsonPath]

  def writeResult(context: TransactionContext, value: JsonNode): TransactionContext = {
    val path = resultPath.getOrElse(States.CONTEXT_ROOT_PATH)
    if (path.getPath == States.CONTEXT_ROOT) {
      context.copy(data = States.PARSER.parse(value))
    } else {
      val newContext = States.PARSER.parse(context.data.read[JsonNode](States.CONTEXT_ROOT_PATH).deepCopy[JsonNode]())
      context.copy(data = setValue(newContext, path, value))
    }
  }


  private def setValue(context: Context, path: JsonPath, value: JsonNode): States.Context = {
    val States.PATH_PATTERN(parentPath, key) = path.getPath
    var theContext = context

    Try(theContext.read(parentPath)).recoverWith {
      case _: PathNotFoundException =>
        theContext = setValue(theContext, JsonPath.compile(parentPath), States.emptyNode)
        Success(true)

      case ex => throw StateException.ResultMappingException(ex, path, context)
    }
    theContext.put(parentPath, key, value)
  }
}
