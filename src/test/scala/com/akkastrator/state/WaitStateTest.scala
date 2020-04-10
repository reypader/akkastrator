package com.akkastrator.state

import java.time.OffsetDateTime
import java.util.UUID

import com.akkastrator.state.States.{Action, Decision, TransactionContext}
import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class WaitStateTest extends AsyncFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  val data: TransactionContext = TransactionContext(UUID.randomUUID(), Step.PARSER.parse(
    """
      {
        "foo": "bar",
        "baz" : 6,
        "bam" : "2029-12-12T12:00:00Z",
        "gen" : {
          "a" : "b"
        }
      }
      """
  ), "test")


  "WaitState.seconds" should "attempt to wait the provided seconds" in {
    val underTest = WaitState(seconds = Some(3), secondsPath = None, timestamp = None, timestampPath = None, end = true)

    val anchor = OffsetDateTime.now()
    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${anchor.plusSeconds(3).format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)
    newContext.currentState shouldEqual States.END

  }

  it should "attempt to wait until the provided deadline" in {
    val deadline = OffsetDateTime.now().plusSeconds(5)
    val underTest = WaitState(seconds = None, secondsPath = None, timestamp = Some(deadline.toString), timestampPath = None, next = Some("DERP"))


    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${deadline.format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)
    newContext.currentState shouldEqual "DERP"
  }

  it should "attempt to wait the provided seconds from the path" in {
    val underTest = WaitState(seconds = None, secondsPath = Some(JsonPath.compile("$.baz")), timestamp = None, timestampPath = None, end = true)

    val anchor = OffsetDateTime.now()
    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${anchor.plusSeconds(6).format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)
    newContext.currentState shouldEqual States.END
  }

  it should "attempt to wait until the provided deadline from the path" in {
    val deadline = OffsetDateTime.parse("2029-12-12T12:00:00Z")
    val underTest = WaitState(seconds = None, secondsPath = None, timestamp = None, timestampPath = Some(JsonPath.compile("$.bam")), end = true, outputPath = JsonPath.compile("$.gen"))


    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${deadline.format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](Step.CONTEXT_ROOT)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual om.readTree(
      """
          {
          "a" : "b"
        }
         """)

    newContext.currentState shouldEqual States.END


  }

}
