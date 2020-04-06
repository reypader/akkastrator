package com.akkastrator.state

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, ValueNode}
import com.jayway.jsonpath.{JsonPath, PathNotFoundException}
import com.typesafe.scalalogging.LazyLogging

import scala.util.Try
import scala.util.matching.Regex

case class PassState(result: Option[JsonNode], resultPath: JsonPath = State.CONTEXT_ROOT) extends State("Pass") with LazyLogging {

  private def extractValue(result: JsonNode): JsonNode =
    result match {
      case value: ObjectNode => result
      case value: ArrayNode => throw new UnsupportedOperationException()
      case value: ValueNode => throw new UnsupportedOperationException()
      case _ => throw new UnsupportedOperationException()
    }

  val pathPattern: Regex = "(^\\$.*)\\['([a-zA-Z0-9_-]+)'\\]".r

  override def decide(context: Context): Try[Context] = Try {
    val input: ObjectNode = context.read(State.CONTEXT_ROOT)
    val value: JsonNode = result.map(extractValue).getOrElse(input.deepCopy())
    if (resultPath == State.CONTEXT_ROOT) {
      State.PARSER.parse(value)
    } else {
      setValue(context, resultPath, value)
    }
  }


  private def setValue(context: Context, path: JsonPath, value: JsonNode): Context = {
    val pathPattern(parentPath, key) = path.getPath
    var theContext = context
    if (theContext.read(parentPath) == null) {
      theContext = setValue(theContext, JsonPath.compile(parentPath), State.EMPTY_NODE)
    }
    theContext.put(parentPath, key, value)
  }
}
