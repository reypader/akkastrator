package com.akkastrator.state

import java.time.OffsetDateTime

import com.akkastrator.state.common.Step.FreeStep
import com.akkastrator.state.common.{Input, Output, Step, TerminalStep}
import com.fasterxml.jackson.databind.node.{IntNode, TextNode}
import com.jayway.jsonpath.JsonPath

import scala.concurrent.{ExecutionContext, Future}

object WaitStep {
  var waitProvider: WaitProvider = _
}

case class WaitStep(seconds: Option[Int],
                    timestamp: Option[String],
                    secondsPath: Option[JsonPath],
                    timestampPath: Option[JsonPath],
                    inputPath: JsonPath = Step.CONTEXT_ROOT,
                    outputPath: JsonPath = Step.CONTEXT_ROOT,
                    next: Option[String] = None,
                    end: Boolean = false)
  extends FreeStep("Wait", next, end) with Input with Output {

  if (List(seconds.isDefined, timestamp.isDefined, secondsPath.isDefined, timestampPath.isDefined).count(b => b) != 1) {
    throw new IllegalArgumentException("There must be exactly one provided among seconds, timestamp, secondsPath, and timestampPath")
  }


  override def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = {
    if (WaitStep.waitProvider == null) {
      throw new IllegalStateException("No waitProvider specified")
    }
    val deadline = (seconds, timestamp, secondsPath, timestampPath) match {
      case (Some(duration), None, None, None) =>
        OffsetDateTime.now().plusSeconds(duration)
      case (None, Some(deadline), None, None) =>
        OffsetDateTime.parse(deadline)
      case (None, None, Some(durationPath), None) =>
        val effectiveInput = getInput(context)
        val duration = effectiveInput.read(durationPath).asInstanceOf[IntNode]
        OffsetDateTime.now().plusSeconds(duration.intValue())
      case (None, None, None, Some(deadlinePath)) =>
        val effectiveInput = getInput(context)
        val deadline = effectiveInput.read(deadlinePath).asInstanceOf[TextNode]
        OffsetDateTime.parse(deadline.textValue())
      case _ => throw new IllegalArgumentException("There must be exactly one provided among seconds, timestamp, secondsPath, and timestampPath")
    }

    WaitStep.waitProvider.waitUntil(deadline).map(_ => (getNext, getOutput(context)))
  }



}
