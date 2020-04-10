package com.akkastrator.state

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.{LogicalConditions, StringConditions}
import com.fasterxml.jackson.databind.node.TextNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AsyncFlatSpec

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
    val underTest = ChoiceStep(choices = List(
      StringConditions.TopStringEquals(JsonPath.compile("$.foo"), "bar", "NEXT")
    ), Some("DEFAULT"))

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "NEXT"
    }
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val underTest = ChoiceStep(choices = List(
      StringConditions.TopStringEquals(JsonPath.compile("$.foo"), "baz", "NEXT")
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
