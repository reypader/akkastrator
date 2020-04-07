package com.akkastrator.state

import com.akkastrator.state.common.{Output, Parameter, Result, State}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, ValueNode}
import com.jayway.jsonpath.JsonPath

import scala.util.Try

case class PassState(result: Option[JsonNode],
                     parameter: Option[JsonNode] = None,
                     inputPath: JsonPath = State.CONTEXT_ROOT,
                     resultPath: JsonPath = State.CONTEXT_ROOT,
                     outputPath: JsonPath = State.CONTEXT_ROOT)
  extends State("Pass") with Parameter with Result with Output {


  override def decide(context: Context): Try[Context] = Try {
    val value: JsonNode = result.getOrElse {
      val input = getInput(context)
      input.deepCopy().asInstanceOf[JsonNode]
    }
    val newContext = writeResult(context, value)
    getOutput(newContext)
  }

}
