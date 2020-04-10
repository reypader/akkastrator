package com.akkastrator.state.common

import com.akkastrator.state.common.Step.Step
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Output {
  def outputPath: JsonPath

  def getOutput(context: Step#Context): Step#Context = if (outputPath.getPath == Step.CONTEXT_ROOT.getPath) {
    context
  } else {
    val output: JsonNode = context.read(outputPath)
    Step.PARSER.parse(output)
  }
}
