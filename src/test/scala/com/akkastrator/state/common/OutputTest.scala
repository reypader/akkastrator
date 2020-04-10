package com.akkastrator.state.common

import java.util.UUID

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OutputTest extends AnyFlatSpec with Matchers {
  val om = new ObjectMapper()
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


  "Output=$" should "return the entire context" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.getOutput(data)

    result.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
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
      """)
  }

  "Output=$._" should "return a Context of a field from the context" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.getOutput(data)

    result.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
        {
          "gen": "bam"
        }
      """)
  }

  it should "return a Context of a field from the context no matter how deeply nested" in {
    object Fake extends Output {
      override def outputPath: JsonPath = JsonPath.compile("$.pow[1].tin")
    }

    val result = Fake.getOutput(data)

    result.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
        "tam"
      """)
  }

}
