package com.akkastrator.state

import com.akkastrator.state.common.State
import com.fasterxml.jackson.databind.node.ValueNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ChoiceStateTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = State.PARSER.parse(
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
    val underTest = ChoiceState(choices = List(
      ChoiceState.TopStringEquals(JsonPath.compile("$.foo"), om.readTree(""""bar"""").asInstanceOf[ValueNode], "NEXT")
    ), Some("DEFAULT"))

    val (next, resultingContext) = underTest.decide(data).get

    resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
    next shouldEqual "NEXT"
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val underTest = ChoiceState(choices = List(
      ChoiceState.TopStringEquals(JsonPath.compile("$.foo"), om.readTree(""""baz"""").asInstanceOf[ValueNode], "NEXT")
    ), Some("DEFAULT"))

    val (next, resultingContext) = underTest.decide(data).get

    resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
    next shouldEqual "DEFAULT"
  }

  "Not StringEquals" should "match the string exactly and return (next, context)" in {
    val underTest = ChoiceState(choices = List(
      ChoiceState.TopNot(ChoiceState.StringEquals(JsonPath.compile("$.foo"), om.readTree(""""baz"""").asInstanceOf[ValueNode]), "NEXT")
    ), Some("DEFAULT"))

    val (next, resultingContext) = underTest.decide(data).get

    resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
    next shouldEqual "NEXT"
  }

  it should "fail to match the string exactly and return (default, context)" in {
    val underTest = ChoiceState(choices = List(
      ChoiceState.TopNot(ChoiceState.StringEquals(JsonPath.compile("$.foo"), om.readTree(""""bar"""").asInstanceOf[ValueNode]), "NEXT")
    ), Some("DEFAULT"))

    val (next, resultingContext) = underTest.decide(data).get

    resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
    next shouldEqual "DEFAULT"
  }
}
