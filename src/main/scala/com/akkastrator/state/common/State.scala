package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, DocumentContext, JsonPath, ParseContext, Option => JsonPathOption}
import com.typesafe.scalalogging.LazyLogging

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object State {
  val PATH_PATTERN: Regex = "(^\\$.*)\\['([a-zA-Z0-9_-]+)'\\]".r
  val CONTEXT_ROOT: JsonPath = JsonPath.compile("$")
  val PARSER: ParseContext = JsonPath.using(
    Configuration
      .builder()
      .mappingProvider(new JacksonMappingProvider())
      .jsonProvider(new JacksonJsonNodeJsonProvider())
      .build())
  val EMPTY_NODE: JsonNode = PARSER.parse("{}").read(CONTEXT_ROOT)
}

abstract class State(stateType: String) extends LazyLogging {
  type Context = DocumentContext

  def decide(context: Context): Try[(String, Context)] = Success((TerminalState.END, context))

  final def perform(context: Context): Try[(String, Context)] = decide(context) recoverWith {
    case e: Throwable =>
      logger.error("encountered exception", e)
      e.printStackTrace()
      Failure(e)
  }
}

