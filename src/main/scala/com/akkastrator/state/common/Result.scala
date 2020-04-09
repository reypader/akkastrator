package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.akkastrator.state.common.State.State
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.{JsonPath, PathNotFoundException}

import scala.util.{Success, Try}

trait Result {
  def resultPath: JsonPath

  def writeResult(context: State#Context, value: JsonNode): State#Context = if (resultPath.getPath == State.CONTEXT_ROOT.getPath) {
    State.PARSER.parse(value)
  } else {
    var newVal = value
    if(context.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode] == value){
      newVal = value.deepCopy()
    }
    setValue(context, resultPath, newVal)
  }


  private def setValue(context: State#Context, path: JsonPath, value: JsonNode): State#Context = {
    val State.PATH_PATTERN(parentPath, key) = path.getPath
    var theContext = context

    Try(theContext.read(parentPath)).recoverWith {
      case _: PathNotFoundException =>
        theContext = setValue(theContext, JsonPath.compile(parentPath), State.emptyNode)
        Success(true)

      case ex => throw StateException.ResultMappingException(ex, path, context)
    }
    theContext.put(parentPath, key, value)
  }
}
