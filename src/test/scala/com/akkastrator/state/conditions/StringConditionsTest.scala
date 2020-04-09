package com.akkastrator.state.conditions

import com.akkastrator.state.common.State
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
    data = State.PARSER.parse(
      """
                {
                  "foo": "baz",
                  "bar": "bam"
                }
                """
    )
  }

  "StringEquals" should "evaluate to false if not equal" in {
    val underTest1 = StringConditions.StringEquals(JsonPath.compile("$.foo"), om.readTree(""""bam"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringEquals(JsonPath.compile("$.bar"), om.readTree(""""baz"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = StringConditions.StringEquals(JsonPath.compile("$.foo"), om.readTree(""""baz"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringEquals(JsonPath.compile("$.bar"), om.readTree(""""bam"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "StringLessThan" should "evaluate to false if not less than" in {
    val underTest1 = StringConditions.StringLessThan(JsonPath.compile("$.foo"), om.readTree(""""bay"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringLessThan(JsonPath.compile("$.bar"), om.readTree(""""bam"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if less than" in {
    val underTest1 = StringConditions.StringLessThan(JsonPath.compile("$.foo"), om.readTree(""""ba}"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringLessThan(JsonPath.compile("$.bar"), om.readTree(""""ban"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "StringGreaterThan" should "evaluate to false if not greater than" in {
    val underTest1 = StringConditions.StringGreaterThan(JsonPath.compile("$.foo"), om.readTree(""""ba}"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringGreaterThan(JsonPath.compile("$.bar"), om.readTree(""""bam"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if greater than" in {
    val underTest1 = StringConditions.StringGreaterThan(JsonPath.compile("$.foo"), om.readTree(""""baZ"""").asInstanceOf[TextNode])
    val underTest2 = StringConditions.StringGreaterThan(JsonPath.compile("$.bar"), om.readTree(""""bal"""").asInstanceOf[TextNode])

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
