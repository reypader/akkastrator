package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, DocumentContext, JsonPath, ParseContext}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex

object Step {
  val PATH_PATTERN: Regex = "(^\\$.*)\\['([a-zA-Z0-9_-]+)'\\]".r
  val CONTEXT_ROOT: JsonPath = JsonPath.compile("$")
  val PARSER: ParseContext = JsonPath.using(
    Configuration
      .builder()
      .mappingProvider(new JacksonMappingProvider())
      .jsonProvider(new JacksonJsonNodeJsonProvider())
      .build())

  def emptyNode: JsonNode = PARSER.parse("{}").read(CONTEXT_ROOT)

  abstract class FreeStep(stepType: String, next: Option[String] = None, end: Boolean = false) extends Step(stepType) {
    if (end && next.isDefined) {
      throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
    }
    if (!end && next.isEmpty) {
      throw new IllegalArgumentException("`next` step must be defined if `end` is false")
    }

    def getNext: String = if (end) TerminalStep.END else next.get

  }

}

abstract class Step(stateType: String, comment: Option[String] = None) extends LazyLogging {
  type Context = DocumentContext

  def perform(context: Context)(implicit executionContext: ExecutionContext): Future[(String, Context)] = Future.successful(TerminalStep.END, context)
}