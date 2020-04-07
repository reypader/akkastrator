package com.akkastrator.state

import com.akkastrator.state.common._
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

import scala.util.Try

case class PassState(result: Option[JsonNode],
                     parameter: Option[JsonNode] = None,
                     inputPath: JsonPath = State.CONTEXT_ROOT,
                     resultPath: JsonPath = State.CONTEXT_ROOT,
                     outputPath: JsonPath = State.CONTEXT_ROOT,
                     next: Option[String] = None,
                     end: Boolean = false)
  extends FreeState("Pass", next, end) with Parameter with Result with Output {


  override def decide(context: Context): Try[(String, Context)] = Try {
    val value: JsonNode = result.getOrElse {
      val input = getInput(context)
      input.deepCopy().asInstanceOf[JsonNode]
    }
    val newContext = writeResult(context, value)
    (getNext, getOutput(newContext))
  }

  private def getNext = if (end) TerminalState.END else next.get

}
