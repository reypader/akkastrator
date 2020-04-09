package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Input {
  def inputPath: JsonPath

  def getInput(context: State#Context): State#Context = State.PARSER.parse(context.read(inputPath).asInstanceOf[JsonNode])
}
