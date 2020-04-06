package com.akkastrator.state

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, DocumentContext, JsonPath, ParseContext}
import com.jayway.jsonpath.{Option => JsonPathOption}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.{Logger, LoggerFactory}

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object State {
  val PATH_PATTERN: Regex = "(^\\$.*)\\['([a-zA-Z0-9_-]+)'\\]".r
  val CONTEXT_ROOT: JsonPath = JsonPath.compile("$")
  val PARSER: ParseContext = JsonPath.using(
    Configuration
      .builder()
      .options(JsonPathOption.SUPPRESS_EXCEPTIONS)
      .mappingProvider(new JacksonMappingProvider())
      .jsonProvider(new JacksonJsonNodeJsonProvider())
      .build())
  val EMPTY_NODE: JsonNode = PARSER.parse("{}").read(CONTEXT_ROOT)
}

abstract class State(stateType: String, end: Boolean = false) extends LazyLogging{
  type Context = DocumentContext

  def decide(context: Context): Try[Context] = Success(context)

  final def perform(context: Context): Try[Context] = decide(context) recoverWith {
    case e: Throwable =>
      logger.error("encountered exception", e)
      e.printStackTrace()
      Failure(e)
  }
}
