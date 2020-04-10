package com.akkastrator.state.common

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, ValueNode}
import com.jayway.jsonpath.JsonPath
import com.typesafe.scalalogging.LazyLogging

trait Parameter extends Input with LazyLogging {

  def parameters: Option[JsonNode]

  override def getInput(context: Step#Context): Step#Context = {
    val effectiveInput = super.getInput(context)
    parameters.map(p => Step.PARSER.parse(assignValues(context, effectiveInput, p))).getOrElse(effectiveInput)
  }

  private def assignValues[T <: JsonNode](originalContext: Step#Context, context: Step#Context, result: T): T =
    result match {
      case value: ObjectNode =>
        var copy = value.deepCopy()
        value.fields.forEachRemaining(entry => {
          logger.info("Reading " + entry.getKey)
          if (entry.getKey.endsWith(".$")) {
            logger.info("Removing " + entry.getKey)
            copy = copy.without(entry.getKey)
            val ref = entry.getValue.textValue()
            val newVal: JsonNode = if (ref.startsWith("$$")) {
              val path = JsonPath.compile(ref.substring(1))
              originalContext.read(path)
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
        copy.asInstanceOf[T]
      case value: ArrayNode =>
        val copy = value.deepCopy()
        for (i <- 0 until value.size()) {
          val newVal = assignValues(originalContext, context, value.get(i))
          copy.set(i, newVal)
        }
        copy.asInstanceOf[T]
      case value: ValueNode => value.asInstanceOf[T]
      case _ => throw new UnsupportedOperationException()
    }
}
