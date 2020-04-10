package com.akkastrator.state.common

import java.util.UUID

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResultTest extends AnyFlatSpec with Matchers {
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


  "Result=$" should "replace the entire context" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.writeResult(data, om.readTree(
      """
        {
          "gen": "bam"
        }
        """))

    result.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
        {
          "gen": "bam"
        }
      """)
  }

  "Result=$._" should "replace a field from the context" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.writeResult(data, om.readTree(
      """
          "derp"
        """))

    result.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
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
      """)
  }

  it should "return a Context of a field from the context no matter how deeply nested" in {
    object Fake extends Result {
      override def resultPath: JsonPath = JsonPath.compile("$.pow[1].tin")
    }

    val result = Fake.writeResult(data, om.readTree(
      """
          {
           "gen": "bam"
          }
        """))

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
          "tin" : {
           "gen": "bam"
          }
        }
        ]
      }
      """)
  }

}
