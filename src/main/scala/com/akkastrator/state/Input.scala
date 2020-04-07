package com.akkastrator.state

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Input {
  def inputPath: JsonPath

  def getInput(context: State#Context): JsonNode = context.read(inputPath).asInstanceOf[JsonNode]
}
