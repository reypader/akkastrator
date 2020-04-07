package com.akkastrator.state.common

import com.fasterxml.jackson.databind.ObjectMapper
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
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


  "Input=$" should "return the entire JsonNode from the context" in {
    object Fake extends Input {
      override def inputPath: JsonPath = JsonPath.compile("$")
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree("""
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

  "Input=$._" should "return a JsonNode of a field from the context" in {
    object Fake extends Input {
      override def inputPath: JsonPath = JsonPath.compile("$.baz")
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree("""
        {
          "gen": "bam"
        }
      """)
  }

  it should "return a JsonNode of a field from the context no matter how deeply nested" in {
    object Fake extends Input {
      override def inputPath: JsonPath = JsonPath.compile("$.pow[1].tin")
    }

    val result = Fake.getInput(data)

    result shouldEqual om.readTree("""
        "tam"
      """)
  }

}
