package com.akkastrator.state

import com.fasterxml.jackson.databind.JsonNode
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OutputTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
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


  "Output=$" should "return the entire context" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.getOutput(data)

    result.read[JsonNode](State.CONTEXT_ROOT) shouldEqual State.PARSER.parse(
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
      """).read[JsonNode](State.CONTEXT_ROOT)
  }

  "Output=$._" should "return a Context of a field from the context" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.getOutput(data)

    result.read[JsonNode](State.CONTEXT_ROOT) shouldEqual State.PARSER.parse(
      """
        {
          "gen": "bam"
        }
      """).read[JsonNode](State.CONTEXT_ROOT)
  }

  it should "return a Context of a field from the context no matter how deeply nested" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$.pow[1].tin")
    }

    val result = Fake.getOutput(data)

    result.read[JsonNode](State.CONTEXT_ROOT) shouldEqual State.PARSER.parse(
      """
        "tam"
      """).read[JsonNode](State.CONTEXT_ROOT)
  }

}
