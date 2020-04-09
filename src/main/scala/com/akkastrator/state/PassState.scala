package com.akkastrator.state

import com.akkastrator.state.common.State.FreeState
import com.akkastrator.state.common._
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class PassState(result: Option[JsonNode],
                     parameter: Option[JsonNode] = None,
                     inputPath: JsonPath = State.CONTEXT_ROOT,
                     resultPath: JsonPath = State.CONTEXT_ROOT,
                     outputPath: JsonPath = State.CONTEXT_ROOT,
                     next: Option[String] = None,
                     end: Boolean = false)
  extends FreeState("Pass", next, end) with Parameter with Result with Output {


  def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] =
    Future.fromTry(Try {
      val effectiveInput: Context = result.map(State.PARSER.parse(_)).getOrElse(getInput(context))
      val newContext = writeResult(context, effectiveInput.read(State.CONTEXT_ROOT))
      (getNext, getOutput(newContext))
    })
}
