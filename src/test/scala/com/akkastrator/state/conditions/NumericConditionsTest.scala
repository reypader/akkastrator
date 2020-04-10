package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.ObjectMapper
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumericConditionsTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = Step.PARSER.parse(
      """
                {
                  "foo": 1.23,
                  "bar": 5
                }
                """
    )
  }

  "NumericEquals" should "evaluate to false if not equal" in {
    val underTest1 = NumericConditions.NumericEquals(JsonPath.compile("$.foo"), BigDecimal("1.24"))
    val underTest2 = NumericConditions.NumericEquals(JsonPath.compile("$.bar"), BigDecimal("-5"))

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = NumericConditions.NumericEquals(JsonPath.compile("$.foo"), BigDecimal("1.23"))
    val underTest2 = NumericConditions.NumericEquals(JsonPath.compile("$.bar"), BigDecimal("5"))

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "NumericLessThan" should "evaluate to false if not less than" in {
    val underTest1 = NumericConditions.NumericLessThan(JsonPath.compile("$.foo"), BigDecimal("1.22"))
    val underTest2 = NumericConditions.NumericLessThan(JsonPath.compile("$.bar"), BigDecimal("-5"))

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if less than" in {
    val underTest1 = NumericConditions.NumericLessThan(JsonPath.compile("$.foo"), BigDecimal("1.24"))
    val underTest2 = NumericConditions.NumericLessThan(JsonPath.compile("$.bar"), BigDecimal("50"))

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "NumericGreaterThan" should "evaluate to false if not greater than" in {
    val underTest1 = NumericConditions.NumericGreaterThan(JsonPath.compile("$.foo"), BigDecimal("1.231"))
    val underTest2 = NumericConditions.NumericGreaterThan(JsonPath.compile("$.bar"), BigDecimal("5"))

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if greater than" in {
    val underTest1 = NumericConditions.NumericGreaterThan(JsonPath.compile("$.foo"), BigDecimal("1.2299"))
    val underTest2 = NumericConditions.NumericGreaterThan(JsonPath.compile("$.bar"), BigDecimal("4.999"))

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
