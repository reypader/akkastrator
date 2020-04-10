package com.akkastrator.state

import java.util.UUID

import com.akkastrator.state.ChoiceState.TopLevelChoice
import com.akkastrator.state.States.{Decision, TransactionContext}
import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.{LogicalConditions, StringConditions}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChoiceStateTest extends AnyFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: TransactionContext = TransactionContext(UUID.randomUUID(), Step.PARSER.parse(
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

  "StringEquals" should "match the string exactly and return (next, context)" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "bar")
    val underTest = ChoiceState(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def evaluate(context: States.Context): Boolean = condition.evaluate(context)
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

        override def evaluate(context: States.Context): Boolean = condition.evaluate(context)
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
        override def next: String = "NEXT"

        override def evaluate(context: States.Context): Boolean = condition.evaluate(context)
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
        override def next: String = "NEXT"

        override def evaluate(context: States.Context): Boolean = condition.evaluate(context)
      }
    ), default = Some("DEFAULT"))

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "DEFAULT"
    }
  }
}
