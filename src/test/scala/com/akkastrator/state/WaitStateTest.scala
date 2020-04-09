package com.akkastrator.state

import java.time.{Duration, OffsetDateTime}

import com.akkastrator.state.common.{State, TerminalState}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.{mock, _}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.Future

class WaitStateTest extends AsyncFlatSpec with Matchers with  BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    val m = mock(classOf[WaitProvider])
    when(m.waitUntil(any(classOf[OffsetDateTime]))).thenReturn(Future.successful(None))
    WaitState.waitProvider = m
    data = State.PARSER.parse(
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
    )
  }

  "WaitState.seconds" should "attempt to wait the provided seconds" in {
    val underTest = WaitState(Some(3), None, None, None, end = true)

    val anchor = OffsetDateTime.now()
    underTest.perform(data) map { result =>
      val captor = ArgumentCaptor.forClass(classOf[OffsetDateTime])
      verify(WaitState.waitProvider).waitUntil(captor.capture())

      val actual = captor.getValue.asInstanceOf[OffsetDateTime]
      Duration.between(anchor,actual ).toSeconds shouldEqual 3
      result._1 shouldEqual TerminalState.END
      result._2.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual data.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode]
    }
  }

  it should "attempt to wait until the provided deadline" in {
    val deadline = OffsetDateTime.now().plusSeconds(5)
    val underTest = WaitState(None, Some(deadline.toString), None, None, next = Some("DERP"))

    underTest.perform(data) map { result =>
      val captor = ArgumentCaptor.forClass(classOf[OffsetDateTime])
      verify(WaitState.waitProvider).waitUntil(captor.capture())

      val actual = captor.getValue.asInstanceOf[OffsetDateTime]
      actual shouldEqual deadline
      result._1 shouldEqual "DERP"
      result._2.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual data.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode]
    }
  }

  it should "attempt to wait the provided seconds from the path" in {
    val underTest = WaitState(None, None, Some(JsonPath.compile("$.baz")), None, end = true)

    val anchor = OffsetDateTime.now()
    underTest.perform(data) map { result =>
      val captor = ArgumentCaptor.forClass(classOf[OffsetDateTime])
      verify(WaitState.waitProvider).waitUntil(captor.capture())

      val actual = captor.getValue.asInstanceOf[OffsetDateTime]
      Duration.between(anchor,actual ).toSeconds shouldEqual 6
      result._1 shouldEqual TerminalState.END
      result._2.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual data.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode]
    }
  }

  it should "attempt to wait until the provided deadline from the path" in {
    val deadline = OffsetDateTime.parse("2029-12-12T12:00:00Z")
    val underTest = WaitState(None, None, None, Some(JsonPath.compile("$.bam")), end = true, outputPath = JsonPath.compile("$.gen"))

    underTest.perform(data) map { result =>
      val captor = ArgumentCaptor.forClass(classOf[OffsetDateTime])
      verify(WaitState.waitProvider).waitUntil(captor.capture())

      val actual = captor.getValue.asInstanceOf[OffsetDateTime]
      actual shouldEqual deadline
      result._1 shouldEqual TerminalState.END
      result._2.read(State.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual om.readTree(
        """
          {
          "a" : "b"
        }
          """)
    }
  }

}
