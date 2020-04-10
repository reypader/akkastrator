package com.akkastrator.state.common

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Input {
  def inputPath: JsonPath

  def getInput(context: TransactionContext): JsonNode = {
    context.data.read[JsonNode](inputPath)
  }
}
