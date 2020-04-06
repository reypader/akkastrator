package com.akkastrator.state

import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, ObjectWriter}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PassStateTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
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

  "No result and resultPath" should "pass the context as-is" in {
    val underTest = PassState(None)

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual data.read[JsonNode]("$")
  }

  "No result and resultPath with inputPath" should "pass the value in inputPath" in {
    val underTest = PassState(None, inputPath = JsonPath.compile("$.baz"))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
  }

  "No result and resultPath with inputPath" should "return the value in outputPath" in {
    val underTest = PassState(None, outputPath = JsonPath.compile("$.baz"))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual om.readTree("""{"gen": "bam"}""")
  }

  "result without resultPath" should "replace the context as an object" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(Some(resultJson))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[ObjectNode]("$") shouldEqual resultJson
    resultingContext.get.read[ObjectNode]("$").get("foo").textValue() shouldEqual "bar"
  }

  it should "replace the context as an array" in {
    val resultJson: JsonNode = om.readTree(
      """
        ["foo", "bar", {"baz":"bam"}]
        """)
    val underTest = PassState(Some(resultJson))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[ArrayNode]("$") shouldEqual resultJson
    resultingContext.get.read[ArrayNode]("$").get(0).textValue() shouldEqual "foo"
    resultingContext.get.read[ArrayNode]("$").get(1).textValue() shouldEqual "bar"
    resultingContext.get.read[ArrayNode]("$").get(2) shouldEqual om.readTree("""{"baz":"bam"}""")
  }

  it should "replace the context as a value" in {
    val resultJson: JsonNode = om.readTree(
      """
        "foo"
        """)
    val underTest = PassState(Some(resultJson))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual resultJson
    resultingContext.get.read[JsonNode]("$").asText() shouldEqual "foo"
  }


  "result with resultPath" should "add to the context" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(Some(resultJson), resultPath = JsonPath.compile("$.result.bar"))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual om.readTree(
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
  }

  "resultPath without result" should "nest to the context inside itself" in {
    val underTest = PassState(None, resultPath = JsonPath.compile("$.result"))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual om.readTree(
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
  }
}
