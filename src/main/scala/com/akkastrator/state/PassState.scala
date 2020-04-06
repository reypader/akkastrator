package com.akkastrator.state

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode, ValueNode}
import com.jayway.jsonpath.JsonPath

import scala.util.Try

case class PassState(result: Option[JsonNode], parameters: Option[JsonNode], inputPath: JsonPath = State.CONTEXT_ROOT, resultPath: JsonPath = State.CONTEXT_ROOT, outputPath: JsonPath = State.CONTEXT_ROOT) extends State("Pass") {

  private def assignValues[T <: JsonNode](context: Context, result: T): T =
    result match {
      case value: ObjectNode =>
        var copy = value.deepCopy()
        value.fields.forEachRemaining(entry => {
          logger.info("Reading " + entry.getKey)
          if (entry.getKey.endsWith(".$")) {
            logger.info("Removing " + entry.getKey)
            copy = copy.without(entry.getKey)
            val path = JsonPath.compile(entry.getValue.textValue())
            val newVal: JsonNode = context.read(path)
            copy = copy.set(entry.getKey.substring(0, entry.getKey.length - 2), newVal)
          } else {
            copy = copy.set(entry.getKey, assignValues(context, entry.getValue))
          }
        }
        )
        copy.asInstanceOf[T]
      case value: ArrayNode =>
        val copy = value.deepCopy()
        for (i <- 0 until value.size()) {
          val newVal = assignValues(context, value.get(i))
          copy.set(i, newVal)
        }
        copy.asInstanceOf[T]
      case value: ValueNode => value.asInstanceOf[T]
      case _ => throw new UnsupportedOperationException()
    }

  override def decide(context: Context): Try[Context] = Try {
    val input: JsonNode = parameters.map(p => assignValues(context, p)).getOrElse(context.read(inputPath).asInstanceOf[JsonNode])
    val value: JsonNode = result.getOrElse(input.deepCopy().asInstanceOf[JsonNode])
    val newContext = if (resultPath == State.CONTEXT_ROOT) {
      State.PARSER.parse(value)
    } else {
      setValue(context, resultPath, value)
    }

    if (outputPath == State.CONTEXT_ROOT) {
      newContext
    } else {
      val output: JsonNode = newContext.read(outputPath)
      State.PARSER.parse(output)
    }
  }


  private def setValue(context: Context, path: JsonPath, value: JsonNode): Context = {
    val State.PATH_PATTERN(parentPath, key) = path.getPath
    var theContext = context
    if (theContext.read(parentPath) == null) {
      theContext = setValue(theContext, JsonPath.compile(parentPath), State.EMPTY_NODE)
    }
    theContext.put(parentPath, key, value)
  }
}
