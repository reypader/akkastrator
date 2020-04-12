package com.akkastrator.state

import com.akkastrator.state.MapState.ParamProxy
import com.akkastrator.state.common.States.{Action, Decision, ErrorHandling, InputOutput, State, TransactionContext, Transition}
import com.akkastrator.state.common.{Parameter, Result, States}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, Reads}

import scala.jdk.CollectionConverters._
import scala.util.Try
import com.akkastrator.state.common.States.jsonPathRead

object MapState {
  implicit val mapStateRead: Reads[MapState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "ResultPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "End").readWithDefault(false) and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Parameters").readNullable[JsonNode] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Retry").readNullable[List[ErrorRetry]] and
      (JsPath \ "Catch").readNullable[List[ErrorCatch]] and
      (JsPath \ "Iterator").read[StateMachine] and
      (JsPath \ "ItemsPath").readNullable[JsonPath] and
      (JsPath \ "MaxConcurrency").read[Int]
    ) (MapState.apply _)

  case class ParamProxy(inputPath: Option[JsonPath] = None, parameters: Option[JsonNode]) extends Parameter

}

case class MapState(inputPath: Option[JsonPath] = None,
                    resultPath: Option[JsonPath] = None,
                    outputPath: Option[JsonPath] = None,
                    end: Boolean = false,
                    next: Option[String] = None,
                    parameters: Option[JsonNode] = None,
                    comment: Option[String] = None,
                    errorRetry: Option[List[ErrorRetry]] = None,
                    errorCatch: Option[List[ErrorCatch]] = None,
                    iterator: StateMachine,
                    itemsPath: Option[JsonPath] = None,
                    maxConcurrency: Int = 0)
  extends State("Map", comment) with InputOutput with Result with Transition with ErrorHandling {
  if (iterator == null) {
    throw new IllegalArgumentException("iterator must be specified")
  }

  if (itemsPath == null) {
    throw new IllegalArgumentException("itemsPath must be specified")
  }

  if (maxConcurrency < 0) {
    throw new IllegalArgumentException("maxConcurrency must be non-negative")
  }

  override def prepare(context: TransactionContext): Try[Action] = Try {
    val effectiveInput = super.getInput(context).deepCopy[JsonNode]()
    if (!effectiveInput.isInstanceOf[ArrayNode]) {
      throw new IllegalStateException("element specified by 'itemsPath' must be an array")
    }

    val mapped = effectiveInput.asInstanceOf[ArrayNode].elements().asScala.map(node => {
      ParamProxy(parameters = parameters).getInput(context.copy(data = States.PARSER.parse(node)))
    }).toSeq

    val mappedEffectiveInput = States.OBJECT_MAPPER.createArrayNode().addAll(mapped.asJavaCollection)
    Action(s"internal:map:$maxConcurrency:${context.transactionId}:${context.currentState}", mappedEffectiveInput)
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    val newContext = writeResult(context, data) //TODO: error/retry
    Decision(getOutput(newContext) copy (currentState = getNext))
  }
}