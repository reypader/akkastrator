package com.akkastrator.state.common

import java.util.UUID

import com.akkastrator.state.{ErrorCatch, ErrorRetry}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider
import com.jayway.jsonpath.{Configuration, DocumentContext, JsonPath, ParseContext}
import play.api.libs.json.{Reads, _}

import scala.util.Try
import scala.util.matching.Regex

object States {
  type Context = DocumentContext
  val PATH_PATTERN: Regex = "(^\\$.*)\\['([a-zA-Z0-9_-]+)'\\]".r
  val CONTEXT_ROOT: String = "$"
  val CONTEXT_ROOT_PATH: JsonPath = JsonPath.compile(CONTEXT_ROOT)
  val PARSER: ParseContext = JsonPath.using(Configuration
    .builder()
    .mappingProvider(new JacksonMappingProvider())
    .jsonProvider(new JacksonJsonNodeJsonProvider())
    .build())
  val END: String = "__END__"
  val OBJECT_MAPPER = new ObjectMapper()
  implicit val stringListRead: Reads[List[String]] = Reads.list[String]
  implicit val jsonPathRead: Reads[JsonPath] = (json: JsValue) => Try {
    json match {
      case o: JsString => JsSuccess(JsonPath.compile(o.value))
      case _ => throw new IllegalArgumentException("paths must be strings")
    }
  }.recover(ex => JsError(ex.getMessage)).get

  implicit val jsonNodeRead: Reads[JsonNode] = (m: JsValue) => Try {
    OBJECT_MAPPER.readTree(m.toString())
  }.map(JsSuccess(_)).recover(ex => JsError(ex.getMessage)).get

  def emptyNode: JsonNode = PARSER.parse("{}").read(CONTEXT_ROOT_PATH)

  trait InputOutput extends Input with Output

  trait Computation extends Parameter with Result

  trait Transition {
    def next: Option[String]

    def end: Boolean

    def getNext: String = if (end) States.END else next.get
  }

  trait ErrorHandling {
    def errorRetry: Option[List[ErrorRetry]]

    def errorCatch: Option[List[ErrorCatch]]
  }

  abstract class State(stateType: String, comment: Option[String]) {
    def prepare(context: TransactionContext): Try[Action]

    def decide(context: TransactionContext, data: JsonNode): Try[Decision]
  }

  case class TransactionContext(transactionId: UUID, data: Context, currentState: String)

  case class Action(targetResource: String, payload: JsonNode)

  case class Decision(newContext: TransactionContext)


}
