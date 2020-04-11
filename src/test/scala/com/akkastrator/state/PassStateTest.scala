package com.akkastrator.state

import java.util.UUID

import com.akkastrator.state.common.States
import com.akkastrator.state.common.States.{Decision, TransactionContext}
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PassStateTest extends AnyFlatSpec with Matchers {
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


  "No result and resultPath" should "pass the context as-is" in {
    val underTest = PassState(result = None, end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[JsonNode]("$") shouldEqual data.data.read[JsonNode]("$")
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  "No result and resultPath with inputPath" should "pass the value in inputPath" in {
    val underTest = PassState(result = None, inputPath = Some(JsonPath.compile("$.baz")), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  "No result and resultPath with inputPath" should "return the value in outputPath" in {
    val underTest = PassState(result = None, outputPath =Some( JsonPath.compile("$.baz")), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  "result without resultPath" should "replace the context as an object" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(result = Some(resultJson), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[ObjectNode]("$") shouldEqual resultJson
        resultingContext.data.read[ObjectNode]("$").get("foo").textValue() shouldEqual "bar"
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  it should "replace the context as an array" in {
    val resultJson: JsonNode = om.readTree(
      """
        ["foo", "bar", {"baz":"bam"}]
        """)
    val underTest = PassState(result = Some(resultJson), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[ArrayNode]("$") shouldEqual resultJson
        resultingContext.data.read[ArrayNode]("$").get(0).textValue() shouldEqual "foo"
        resultingContext.data.read[ArrayNode]("$").get(1).textValue() shouldEqual "bar"
        resultingContext.data.read[ArrayNode]("$").get(2) shouldEqual om.readTree("""{"baz":"bam"}""")
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  it should "replace the context as a value" in {
    val resultJson: JsonNode = om.readTree(
      """
        "foo"
        """)
    val underTest = PassState(result = Some(resultJson), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual resultJson
        resultingContext.data.read[JsonNode]("$").asText() shouldEqual "foo"
        resultingContext.currentState shouldEqual "__END__"
    }
  }


  "result with resultPath" should "add to the context" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(result = Some(resultJson), resultPath = Some(JsonPath.compile("$.result.bar")), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[JsonNode]("$") shouldEqual om.readTree(
          """
        {
        "result" : {
          "bar": {
            "foo" : "bar"
          }
        },
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
        """)
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  "resultPath without result" should "nest to the context inside itself" in {
    val underTest = PassState(result = None, resultPath =Some( JsonPath.compile("$.result")), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>
        resultingContext.data.read[JsonNode]("$") shouldEqual om.readTree(
          """
        {
        "result" :  {
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
        },
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
        """)
        resultingContext.currentState shouldEqual "__END__"
    }
  }

  "parameters" should "replace the input" in {
    val params: JsonNode = om.readTree(
      """
        {
          "foo" : "bar",
          "baz.$" : "$.baz",
          "bam" : [
            "gen",
            {
              "pip.$" : "$.pow[1].tin"
            }
          ]
        }
        """)
    val underTest = PassState(result = None, parameters = Some(params), end = true)

    val action = underTest.prepare(data)
    underTest.decide(data, action.get.payload) map {
      case Decision(resultingContext) =>

        resultingContext.data.read[JsonNode]("$") shouldEqual om.readTree(
          """
        {
          "foo" : "bar",
          "baz" : {
            "gen": "bam"
          },
          "bam" : [
            "gen",
            {
              "pip" : "tam"
            }
          ]
        }
        """)
        resultingContext.currentState shouldEqual "__END__"
    }
  }
}
