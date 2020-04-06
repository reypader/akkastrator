package com.akkastrator.state

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, DocumentContext, JsonPath, ParseContext}
import com.jayway.jsonpath.{Option => JsonPathOption}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object State {
  val logger: Logger = LoggerFactory.getLogger(classOf[State])
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

abstract class State(stateType: String, end: Boolean = false) {
  type Context = DocumentContext

  def decide(context: Context): Try[Context] = Success(context)

  final def perform(context: Context): Try[Context] = decide(context) recoverWith {
    case e: Throwable =>
      State.logger.error("encountered exception", e)
      e.printStackTrace()
      Failure(e)
  }
}
