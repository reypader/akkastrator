package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Output {
  def outputPath: JsonPath
  def getOutput(context: State#Context): State#Context = if (outputPath.getPath == State.CONTEXT_ROOT.getPath) {
    context
  } else {
    val output: JsonNode = context.read(outputPath)
    State.PARSER.parse(output)
  }
}
