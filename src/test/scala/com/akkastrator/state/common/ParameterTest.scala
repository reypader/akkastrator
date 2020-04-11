package com.akkastrator.state.common

import java.util.UUID

import com.akkastrator.state.common.States.TransactionContext
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParameterTest extends AnyFlatSpec with Matchers {
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


  "Parameter=None" should "return the entire JsonNode from the context" in {
    object Fake extends Parameter {
      override def parameters: Option[JsonNode] = None

      override def inputPath: Option[JsonPath] = Some(JsonPath.compile("$"))
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree(
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

  "Parameter $ reference" should "replace a field with a referenced field from the effective input $.baz" in {
    object Fake extends Parameter {
      override def parameters: Option[JsonNode] = Some(om.readTree(
        """
          {
            "herp.$" : "$.gen"
          }
          """))

      override def inputPath:  Option[JsonPath] = Some(JsonPath.compile("$.baz"))
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree(
      """
         {
            "herp" : "bam"
          }

      """)
  }

  it should "replace a field with a referenced field from the effective input $" in {
    object Fake extends Parameter {
      override def parameters: Option[JsonNode] = Some(om.readTree(
        """
          {
            "herp.$" : "$.baz"
          }
          """))

      override def inputPath: Option[JsonPath] = Some(JsonPath.compile("$"))
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree(
      """
         {
            "herp" : {
             "gen": "bam"
            }
          }

      """)
  }

  "Parameter with $$ reference" should "replace a field with a referenced field from the effective input $.baz and a context field $$" in {
    object Fake extends Parameter {
      override def parameters: Option[JsonNode] = Some(om.readTree(
        """
          {
            "herp.$" : "$.gen",
            "derp.$" : "$$.pow[1].tin"
          }
          """))

      override def inputPath:  Option[JsonPath] = Some(JsonPath.compile("$.baz"))
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree(
      """
         {
            "herp" : "bam",
            "derp": "tam"
          }

      """)
  }

}
