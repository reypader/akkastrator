package com.akkastrator.state

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

  "result without resultPath" should "replace the context" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(Some(resultJson))

    val resultingContext = underTest.perform(data)

    resultingContext.get.read[JsonNode]("$") shouldEqual resultJson
  }

  "result with resultPath" should "add to the context" in {
    val resultJson: JsonNode = om.readTree(
      """
        {
          "foo" : "bar"
        }
        """)
    val underTest = PassState(Some(resultJson), JsonPath.compile("$.result.bar"))

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
    val underTest = PassState(None, JsonPath.compile("$.result"))

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
