package com.akkastrator.state

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Result {
  def resultPath: JsonPath

  def writeResult(context: State#Context, value: JsonNode): State#Context = if (resultPath.getPath == State.CONTEXT_ROOT.getPath) {
    State.PARSER.parse(value)
  } else {
    setValue(context, resultPath, value)
  }


  private def setValue(context: State#Context, path: JsonPath, value: JsonNode): State#Context = {
    val State.PATH_PATTERN(parentPath, key) = path.getPath
    var theContext = context
    if (theContext.read(parentPath) == null) {
      theContext = setValue(theContext, JsonPath.compile(parentPath), State.EMPTY_NODE)
    }
    theContext.put(parentPath, key, value)
  }
}
