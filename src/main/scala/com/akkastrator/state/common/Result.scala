package com.akkastrator.state.common

import com.akkastrator.state.States._
import com.akkastrator.state.{StateException, States}
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.{JsonPath, PathNotFoundException}

import scala.util.{Success, Try}

trait Result {
  def resultPath: JsonPath

  def writeResult(context: TransactionContext, value: JsonNode): TransactionContext = if (resultPath.getPath == Step.CONTEXT_ROOT.getPath) {
    context.copy(data = Step.PARSER.parse(value))
  } else {
    val newContext = Step.PARSER.parse(context.data.read[JsonNode](Step.CONTEXT_ROOT).deepCopy[JsonNode]())
    context.copy(data = setValue(newContext, resultPath, value))
  }


  private def setValue(context: Context, path: JsonPath, value: JsonNode): States.Context = {
    val Step.PATH_PATTERN(parentPath, key) = path.getPath
    var theContext = context

    Try(theContext.read(parentPath)).recoverWith {
      case _: PathNotFoundException =>
        theContext = setValue(theContext, JsonPath.compile(parentPath), Step.emptyNode)
        Success(true)

      case ex => throw StateException.ResultMappingException(ex, path, context)
    }
    theContext.put(parentPath, key, value)
  }
}
