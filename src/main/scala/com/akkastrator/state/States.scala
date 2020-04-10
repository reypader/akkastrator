package com.akkastrator.state

import java.util.UUID

import com.akkastrator.state.common._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.DocumentContext
import play.api.libs.json.{Reads, _}

import scala.util.Try

object States {
  type Context = DocumentContext
  val END: String = "__END__"
  implicit val stringListRead: Reads[List[String]] = Reads.list[String]

  implicit val jsonNodeRead: Reads[JsonNode] = (m: JsValue) => Try {
    om.readTree(m.toString())
  }.map(JsSuccess(_)).recover(ex => JsError(ex.getMessage)).get
  val om = new ObjectMapper()

  trait InputOutput extends Input with Output

  trait Computation extends Parameter with Result

  trait Transition {
    def next: Option[String]

    def end: Boolean

    def getNext: String = if (end) States.END else next.get
  }

  trait ErrorHandling {
    def errorRetry: Option[ErrorRetry]

    def errorCatch: Option[ErrorCatch]
  }

  abstract class State(stateType: String, comment: Option[String]) {
    def prepare(context: TransactionContext): Try[Action]

    def decide(context: TransactionContext, data: JsonNode): Try[Decision]
  }

  case class TransactionContext(transactionId: UUID, data: Context, currentState: String)

  case class Action(targetResource: String, payload: JsonNode)

  case class Decision(newContext: TransactionContext)


}
