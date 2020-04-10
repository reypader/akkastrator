package com.akkastrator.state.common

import com.akkastrator.state.States.TransactionContext
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Output {
  def outputPath: JsonPath

  def getOutput(context: TransactionContext): TransactionContext = if (outputPath.getPath == Step.CONTEXT_ROOT.getPath) {
    context
  } else {
    val output: JsonNode = context.data.read(outputPath)
    context.copy(data = Step.PARSER.parse(output))
  }
}
