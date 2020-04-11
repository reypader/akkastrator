package com.akkastrator.state.common

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

trait Output {
  def outputPath: Option[JsonPath]

  def getOutput(context: TransactionContext): TransactionContext = {
    val path = outputPath.getOrElse(States.CONTEXT_ROOT_PATH)
    if (path.getPath == States.CONTEXT_ROOT) {
      context
    } else {
      val output: JsonNode = context.data.read(path)
      context.copy(data = States.PARSER.parse(output))
    }
  }
}
