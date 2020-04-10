package com.akkastrator.state.common

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResultTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
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


  "Result=$" should "replace the entire context" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.writeResult(data, om.readTree("""
        {
          "gen": "bam"
        }
        """))

    result.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual Step.PARSER.parse(
      """
        {
          "gen": "bam"
        }
      """).read[JsonNode](Step.CONTEXT_ROOT)
  }

  "Result=$._" should "replace a field from the context" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.writeResult(data, om.readTree("""
          "derp"
        """))

    result.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual Step.PARSER.parse(
      """
        {
        "foo": "bar",
        "baz" : "derp",
        "pow" : [
        "pop",
        {
          "tin" : "tam"
        }
        ]
      }
      """).read[JsonNode](Step.CONTEXT_ROOT)
  }

  it should "return a Context of a field from the context no matter how deeply nested" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$.pow[1].tin")
    }

    val result = Fake.writeResult(data, om.readTree("""
          {
           "gen": "bam"
          }
        """))

    result.read[JsonNode](Step.CONTEXT_ROOT) shouldEqual Step.PARSER.parse(
      """
        {
        "foo": "bar",
        "baz" : {
          "gen": "bam"
        },
        "pow" : [
        "pop",
        {
          "tin" : {
           "gen": "bam"
          }
        }
        ]
      }
      """).read[JsonNode](Step.CONTEXT_ROOT)
  }

}
