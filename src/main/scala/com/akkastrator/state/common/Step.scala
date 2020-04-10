package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, JsonPath, ParseContext}

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

}