package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringConditionsTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = Step.PARSER.parse(
      """
                {
                  "foo": "baz",
                  "bar": "bam"
                }
                """
    )
  }

  "StringEquals" should "evaluate to false if not equal" in {
    val underTest1 = StringConditions.StringEquals(JsonPath.compile("$.foo"), "bam")
    val underTest2 = StringConditions.StringEquals(JsonPath.compile("$.bar"), "baz")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = StringConditions.StringEquals(JsonPath.compile("$.foo"), "baz")
    val underTest2 = StringConditions.StringEquals(JsonPath.compile("$.bar"), "bam")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "StringLessThan" should "evaluate to false if not less than" in {
    val underTest1 = StringConditions.StringLessThan(JsonPath.compile("$.foo"), "bay")
    val underTest2 = StringConditions.StringLessThan(JsonPath.compile("$.bar"), "bam")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if less than" in {
    val underTest1 = StringConditions.StringLessThan(JsonPath.compile("$.foo"), "ba}")
    val underTest2 = StringConditions.StringLessThan(JsonPath.compile("$.bar"), "ban")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "StringGreaterThan" should "evaluate to false if not greater than" in {
    val underTest1 = StringConditions.StringGreaterThan(JsonPath.compile("$.foo"), "ba}")
    val underTest2 = StringConditions.StringGreaterThan(JsonPath.compile("$.bar"), "bam")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if greater than" in {
    val underTest1 = StringConditions.StringGreaterThan(JsonPath.compile("$.foo"), "baZ")
    val underTest2 = StringConditions.StringGreaterThan(JsonPath.compile("$.bar"), "bal")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
