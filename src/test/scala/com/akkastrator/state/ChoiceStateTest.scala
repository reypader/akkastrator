package com.akkastrator.state

import java.util.UUID

import com.akkastrator.state.ChoiceState.{ChoiceRule, TopLevelChoice}
import com.akkastrator.state.StateException.StateFailure
import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Decision, TransactionContext}
import com.akkastrator.state.conditions.{LogicalConditions, StringConditions}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.mockito.Mockito._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChoiceStateTest extends AnyFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: TransactionContext = TransactionContext(UUID.randomUUID(), States.PARSER.parse(
    """
                {
                  "foo": "bar",
                  "baz" : {
                    "gen": "bam"
                  },
                  "pow" : [
                  "pop",
                  {
                    "tin" : "tam"
                  }
                  ]
                }
                """
  ), "test")

  "Empty choices" should "throw an IllegalArgumentException" in {
    assertThrows[IllegalArgumentException]{
      ChoiceState(choices = List(), default = Some(""))
    }
  }

  "null choices" should "throw an IllegalArgumentException" in {
    assertThrows[IllegalArgumentException]{
      ChoiceState(choices = null, default = Some(""))
    }
  }

  "choices with missing next" should "throw an IllegalArgumentException for null" in {
    assertThrows[IllegalArgumentException]{
      ChoiceState(choices = List(new TopLevelChoice {
        override def inner: ChoiceState.ChoiceRule = mock(classOf[ChoiceRule])

        override def next: String = null
      }), default = Some(""))
    }
  }

  it should "throw an IllegalArgumentException for empty" in {
    assertThrows[IllegalArgumentException]{
      ChoiceState(choices = List(new TopLevelChoice {
        override def inner: ChoiceState.ChoiceRule = mock(classOf[ChoiceRule])

        override def next: String = ""
      }), default = Some(""))
    }
  }

  it should "throw an IllegalArgumentException for blank" in {
    assertThrows[IllegalArgumentException]{
      ChoiceState(choices = List(new TopLevelChoice {
        override def inner: ChoiceState.ChoiceRule = mock(classOf[ChoiceRule])

        override def next: String = "  "
      }), default = Some(""))
    }
  }

  "StringEquals" should "match the string exactly and return (next, context)" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "bar")
    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def inner: ChoiceState.ChoiceRule = condition

      }
    ), default = Some("DEFAULT"))

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "NEXT"
    }
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz")
    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def inner: ChoiceState.ChoiceRule = condition
      }
    ), default = Some("DEFAULT"))

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "DEFAULT"
    }
  }

  "Not StringEquals" should "match the string exactly and return (next, context)" in {
    val condition = LogicalConditions.Not(StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz"))
    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def inner: ChoiceState.ChoiceRule = condition

        override def next: String = "NEXT"
      }
    ), default = Some("DEFAULT"))

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "NEXT"
    }
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val condition = LogicalConditions.Not(StringConditions.StringEquals(JsonPath.compile("$.foo"), "bar"))

    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def inner: ChoiceState.ChoiceRule = condition

        override def next: String = "NEXT"
      }
    ), default = Some("DEFAULT"))

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "DEFAULT"
    }
  }

  "No default and no match" should "fail" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz")
    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def inner: ChoiceState.ChoiceRule = condition
      }
    ), default = None)

    val action = underTest.prepare(data)

    val caught = intercept[StateFailure] {
        underTest.decide(data, action.get.payload).get
      }

    caught.error shouldBe "States.NoChoiceMatched"
    caught.cause shouldBe data.currentState
  }
}
