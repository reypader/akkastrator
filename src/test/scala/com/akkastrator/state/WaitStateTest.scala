package com.akkastrator.state

import java.time.OffsetDateTime
import java.util.UUID

import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Action, Decision, TransactionContext}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class WaitStateTest extends AsyncFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: TransactionContext = TransactionContext(UUID.randomUUID(), States.PARSER.parse(
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

  "end/pass" should "throw IllegalArgumentException if both absent" in {
    assertThrows[IllegalArgumentException] {
      WaitState(end = false, next = None, seconds = Some(1), secondsPath = None, timestamp = None, timestampPath = None)
    }
  }
  it should "throw IllegalArgumentException if both present" in {
    assertThrows[IllegalArgumentException] {
      WaitState(end = true, next = Some(""), seconds = Some(1), secondsPath = None, timestamp = None, timestampPath = None)
    }
  }

  "constructor" should "throw IllegalArgumentException if no deadline is provided" in {
    assertThrows[IllegalArgumentException]{
      WaitState(end = true, next = None, seconds = None, secondsPath = None, timestamp = None, timestampPath = None)
    }
  }

  it should "throw IllegalArgumentException if more than 1 deadline is provided" in {
    assertThrows[IllegalArgumentException]{
      WaitState(end = true, next = None, seconds =  Some(1), secondsPath =None, timestamp = Some(OffsetDateTime.now()), timestampPath = None)
    }
  }


  "WaitState.seconds" should "attempt to wait the provided seconds" in {
    val underTest = WaitState(seconds = Some(3), secondsPath = None, timestamp = None, timestampPath = None, end = true)

    val anchor = OffsetDateTime.now()
    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${anchor.plusSeconds(3).format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](States.CONTEXT_ROOT_PATH) shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)
    newContext.currentState shouldEqual States.END

  }

  it should "attempt to wait until the provided deadline" in {
    val deadline = OffsetDateTime.now().plusSeconds(5)
    val underTest = WaitState(seconds = None, secondsPath = None, timestamp = Some(deadline), timestampPath = None, next = Some("DERP"))


    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${deadline.format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](States.CONTEXT_ROOT_PATH) shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)
    newContext.currentState shouldEqual "DERP"
  }

  it should "attempt to wait the provided seconds from the path" in {
    val underTest = WaitState(seconds = None, secondsPath = Some(JsonPath.compile("$.baz")), timestamp = None, timestampPath = None, end = true)

    val anchor = OffsetDateTime.now()
    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${anchor.plusSeconds(6).format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](States.CONTEXT_ROOT_PATH) shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)
    newContext.currentState shouldEqual States.END
  }

  it should "attempt to wait until the provided deadline from the path" in {
    val deadline = OffsetDateTime.parse("2029-12-12T12:00:00Z")
    val underTest = WaitState(seconds = None, secondsPath = None, timestamp = None, timestampPath = Some(JsonPath.compile("$.bam")), end = true, outputPath = Some(JsonPath.compile("$.gen")))


    val Action(targetResource, payload) = underTest.prepare(data).get

    targetResource shouldEqual s"internal:wait:${deadline.format(WaitState.TIMESTAMP_FORMAT)}:execution:${data.transactionId}"
    payload shouldEqual data.data.read[JsonNode](States.CONTEXT_ROOT_PATH)

    val Decision(newContext) = underTest.decide(data, payload).get
    newContext.data.read[JsonNode](States.CONTEXT_ROOT_PATH) shouldEqual om.readTree(
      """
          {
          "a" : "b"
        }
         """)

    newContext.currentState shouldEqual States.END


  }

}
