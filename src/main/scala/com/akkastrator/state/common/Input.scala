package com.akkastrator.state.common

import com.akkastrator.state.common.Step.Step
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Input {
  def inputPath: JsonPath

  def getInput(context: Step#Context): Step#Context = Step.PARSER.parse(context.read(inputPath).asInstanceOf[JsonNode])
}
