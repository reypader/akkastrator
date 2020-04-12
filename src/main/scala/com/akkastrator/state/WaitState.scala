package com.akkastrator.state

import java.time.OffsetDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._

import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Action, Decision, InputOutput, State, TransactionContext, Transition}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{IntNode, TextNode}
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

import scala.util.Try
import com.akkastrator.state.common.States.jsonPathRead

object WaitState {

  val TIMESTAMP_FORMAT: DateTimeFormatter = new DateTimeFormatterBuilder()
    .parseCaseInsensitive
    .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
    .appendLiteral('-')
    .appendValue(MONTH_OF_YEAR, 2)
    .appendLiteral('-')
    .appendValue(DAY_OF_MONTH, 2)
    .appendLiteral('T')
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)
    .appendLiteral(':')
    .appendValue(SECOND_OF_MINUTE, 2)
    .appendOffsetId
    .toFormatter()

  implicit val waitStateRead: Reads[WaitState] = (
    (JsPath \ "InputPath").readNullable[JsonPath] and
      (JsPath \ "OutputPath").readNullable[JsonPath] and
      (JsPath \ "End").readWithDefault(false) and
      (JsPath \ "Next").readNullable[String] and
      (JsPath \ "Comment").readNullable[String] and
      (JsPath \ "Seconds").readNullable[Int] and
      (JsPath \ "Timestamp").readNullable[String].map(s=> s.map(OffsetDateTime.parse(_))) and
      (JsPath \ "SecondsPath").readNullable[String].map(s => s.map(JsonPath.compile(_))) and
      (JsPath \ "TimestampPath").readNullable[String].map(s => s.map(JsonPath.compile(_)))
    ) (WaitState.apply _)

}


case class WaitState(inputPath: Option[JsonPath] = None,
                     outputPath: Option[JsonPath] = None,
                     end: Boolean = false,
                     next: Option[String] = None,
                     comment: Option[String] = None,
                     seconds: Option[Int],
                     timestamp: Option[OffsetDateTime],
                     secondsPath: Option[JsonPath],
                     timestampPath: Option[JsonPath])
  extends State("Wait", comment) with InputOutput with Transition {
  if (end && next.isDefined) {
    throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
  }
  if (!end && next.isEmpty) {
    throw new IllegalArgumentException("`next` step must be defined if `end` is false")
  }
  if (List(seconds.isDefined, timestamp.isDefined, secondsPath.isDefined, timestampPath.isDefined).count(b => b) != 1) {
    throw new IllegalArgumentException("There must be exactly one provided among seconds, timestamp, secondsPath, and timestampPath")
  }

  override def prepare(context: TransactionContext): Try[Action] = Try {
    val deadline = (seconds, timestamp, secondsPath, timestampPath) match {
      case (Some(duration), None, None, None) =>
        OffsetDateTime.now().plusSeconds(duration)
      case (None, Some(deadline), None, None) => deadline
      case (None, None, Some(durationPath), None) =>
        val effectiveInput = States.PARSER.parse(getInput(context))
        val duration = effectiveInput.read[IntNode](durationPath)
        OffsetDateTime.now().plusSeconds(duration.intValue())
      case (None, None, None, Some(deadlinePath)) =>
        val effectiveInput = States.PARSER.parse(getInput(context))
        val deadline = effectiveInput.read[TextNode](deadlinePath)
        OffsetDateTime.parse(deadline.textValue())
      case _ => throw new IllegalArgumentException("There must be exactly one provided among seconds, timestamp, secondsPath, and timestampPath")
    }
    Action(s"internal:wait:${deadline.format(WaitState.TIMESTAMP_FORMAT)}:execution:${context.transactionId}", getInput(context))
  }

  override def decide(context: TransactionContext, data: JsonNode): Try[Decision] = Try {
    Decision(getOutput(context) copy (currentState = getNext))
  }
}
