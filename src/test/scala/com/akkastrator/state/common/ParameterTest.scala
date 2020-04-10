package com.akkastrator.state.common

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParameterTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
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


  "Parameter=None" should "return the entire JsonNode from the context" in {
    object Fake extends Parameter {
      override def parameter: Option[JsonNode] = None

      override def inputPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.getInput(data)

    result.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual om.readTree(
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
      override def parameter: Option[JsonNode] = Some(om.readTree("""
          {
            "herp.$" : "$.gen"
          }
          """))

      override def inputPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.getInput(data)

    result.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual om.readTree(
      """
         {
            "herp" : "bam"
          }

      """)
  }

  it should "replace a field with a referenced field from the effective input $" in {
    object Fake extends Parameter {
      override def parameter: Option[JsonNode] = Some(om.readTree("""
          {
            "herp.$" : "$.baz"
          }
          """))

      override def inputPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.getInput(data)

    result.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual om.readTree(
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
      override def parameter: Option[JsonNode] = Some(om.readTree("""
          {
            "herp.$" : "$.gen",
            "derp.$" : "$$.pow[1].tin"
          }
          """))

      override def inputPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.getInput(data)

    result.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode] shouldEqual om.readTree(
      """
         {
            "herp" : "bam",
            "derp": "tam"
          }

      """)
  }

}
