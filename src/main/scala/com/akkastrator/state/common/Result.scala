package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.{JsonPath, PathNotFoundException}

import scala.util.{Success, Try}

trait Result {
  def resultPath: JsonPath

  def writeResult(context: Step#Context, value: JsonNode): Step#Context = if (resultPath.getPath == Step.CONTEXT_ROOT.getPath) {
    Step.PARSER.parse(value)
  } else {
    var newVal = value
    if (context.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode] == value) {
      newVal = value.deepCopy()
    }
    setValue(context, resultPath, newVal)
  }


  private def setValue(context: Step#Context, path: JsonPath, value: JsonNode): Step#Context = {
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
