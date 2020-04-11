package com.akkastrator.state.common

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Input {
  def inputPath: Option[JsonPath]

  def getInput(context: TransactionContext): JsonNode = {
    val path = inputPath.getOrElse(States.CONTEXT_ROOT_PATH)
    context.data.read[JsonNode](path)
  }
}
