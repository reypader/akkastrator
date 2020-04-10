package com.akkastrator.state

import com.akkastrator.state.common.Step.FreeStep
import com.akkastrator.state.common._
import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class PassStep(result: Option[JsonNode],
                    parameter: Option[JsonNode] = None,
                    inputPath: JsonPath = Step.CONTEXT_ROOT,
                    resultPath: JsonPath = Step.CONTEXT_ROOT,
                    outputPath: JsonPath = Step.CONTEXT_ROOT,
                    next: Option[String] = None,
                    end: Boolean = false)
  extends FreeStep("Pass", next, end) with Parameter with Result with Output {


  def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] =
    Future.fromTry(Try {
      val effectiveInput: Context = result.map(Step.PARSER.parse(_)).getOrElse(getInput(context))
      val newContext = writeResult(context, effectiveInput.read(Step.CONTEXT_ROOT))
      (getNext, getOutput(newContext))
    })
}
