package com.akkastrator.state

import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class PassStepTest extends AsyncFlatSpec with Matchers with BeforeAndAfterEach {
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

  "No result and resultPath" should "pass the context as-is" in {
    val underTest = PassStep(None, None, end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
        next shouldEqual "__END__"
    }
  }

  "No result and resultPath with inputPath" should "pass the value in inputPath" in {
    val underTest = PassStep(None, None, inputPath = JsonPath.compile("$.baz"), end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
        next shouldEqual "__END__"
    }
  }

  "No result and resultPath with inputPath" should "return the value in outputPath" in {
    val underTest = PassStep(None, None, outputPath = JsonPath.compile("$.baz"), end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
        next shouldEqual "__END__"
    }
  }

  "result without resultPath" should "replace the context as an object" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassStep(Some(resultJson), None, end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[ObjectNode]("$") shouldEqual resultJson
        resultingContext.read[ObjectNode]("$").get("foo").textValue() shouldEqual "bar"
        next shouldEqual "__END__"
    }
  }

  it should "replace the context as an array" in {
    val resultJson: JsonNode = om.readTree(
      """
        ["foo", "bar", {"baz":"bam"}]
        """)
    val underTest = PassStep(Some(resultJson), None, end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[ArrayNode]("$") shouldEqual resultJson
        resultingContext.read[ArrayNode]("$").get(0).textValue() shouldEqual "foo"
        resultingContext.read[ArrayNode]("$").get(1).textValue() shouldEqual "bar"
        resultingContext.read[ArrayNode]("$").get(2) shouldEqual om.readTree("""{"baz":"bam"}""")
        next shouldEqual "__END__"
    }
  }

  it should "replace the context as a value" in {
    val resultJson: JsonNode = om.readTree(
      """
        "foo"
        """)
    val underTest = PassStep(Some(resultJson), None, end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual resultJson
        resultingContext.read[JsonNode]("$").asText() shouldEqual "foo"
        next shouldEqual "__END__"
    }
  }


  "result with resultPath" should "add to the context" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassStep(Some(resultJson), None, resultPath = JsonPath.compile("$.result.bar"), end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[JsonNode]("$") shouldEqual om.readTree(
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
        next shouldEqual "__END__"
    }
  }

  "resultPath without result" should "nest to the context inside itself" in {
    val underTest = PassStep(None, None, resultPath = JsonPath.compile("$.result"), end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>
        resultingContext.read[JsonNode]("$") shouldEqual om.readTree(
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
        next shouldEqual "__END__"
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
    val underTest = PassStep(None, Some(params), end = true)

    underTest.perform(data) map {
      case (next, resultingContext) =>

        resultingContext.read[JsonNode]("$") shouldEqual om.readTree(
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
        next shouldEqual "__END__"
    }
  }
}