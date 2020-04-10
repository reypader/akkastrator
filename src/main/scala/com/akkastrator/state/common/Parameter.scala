package com.akkastrator.state.common

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, ValueNode}
import com.jayway.jsonpath.JsonPath
import com.typesafe.scalalogging.LazyLogging

trait Parameter extends Input with LazyLogging {

  def parameters: Option[JsonNode]

  override def getInput(context: TransactionContext): JsonNode = {
    val effectiveInput = super.getInput(context)
    parameters.map(p => assignValues(context, States.PARSER.parse(effectiveInput), p)).getOrElse(effectiveInput)
  }

  private def assignValues[T <: JsonNode](originalContext: TransactionContext, context: States.Context, result: T): JsonNode =
    result match {
      case value: ObjectNode =>
        var copy = value.deepCopy[ObjectNode]()
        value.fields.forEachRemaining(entry => {
          logger.info("Reading " + entry.getKey)
          if (entry.getKey.endsWith(".$")) {
            logger.info("Removing " + entry.getKey)
            copy = copy.without(entry.getKey)
            val ref = entry.getValue.textValue()
            val newVal: JsonNode = if (ref.startsWith("$$")) {
              val path = JsonPath.compile(ref.substring(1))
              originalContext.data.read(path)
            } else {
              val path = JsonPath.compile(ref)
              context.read(path)
            }
            copy = copy.set(entry.getKey.substring(0, entry.getKey.length - 2), newVal)
          } else {
            copy = copy.set(entry.getKey, assignValues(originalContext, context, entry.getValue))
          }
        }
        )
        copy
      case value: ArrayNode =>
        val copy = value.deepCopy[ArrayNode]()
        for (i <- 0 until value.size()) {
          val newVal = assignValues(originalContext, context, value.get(i))
          copy.set(i, newVal)
        }
        copy
      case value: ValueNode => value
      case _ => throw new UnsupportedOperationException()
    }
}
