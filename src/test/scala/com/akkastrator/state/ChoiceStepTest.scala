package com.akkastrator.state

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices.TopLevelChoice
import com.akkastrator.state.conditions.{LogicalConditions, StringConditions}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class ChoiceStepTest extends AsyncFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = Step.PARSER.parse(
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
    )
  }

  "StringEquals" should "match the string exactly and return (next, context)" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "bar")
    val underTest = ChoiceStep(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def evaluate(context: Step#Context): Boolean = condition.evaluate(context)
      }
    ), Some("DEFAULT"))

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "NEXT"
    }
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val condition = StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz")
    val underTest = ChoiceStep(choices = List(
      new TopLevelChoice {
        override def next: String = "NEXT"

        override def evaluate(context: Step#Context): Boolean = condition.evaluate(context)
      }
    ), Some("DEFAULT"))

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "DEFAULT"
    }
  }

  "Not StringEquals" should "match the string exactly and return (next, context)" in {
    val underTest = ChoiceStep(choices = List(
      LogicalConditions.TopNot(StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz"), "NEXT")
    ), Some("DEFAULT"))

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "NEXT"
    }
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val underTest = ChoiceStep(choices = List(
      LogicalConditions.TopNot(StringConditions.StringEquals(JsonPath.compile("$.foo"), "bar"), "NEXT")
    ), Some("DEFAULT"))

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "DEFAULT"
    }
  }
}
