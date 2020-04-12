package com.akkastrator.state.conditions

import com.akkastrator.state.common.States
import com.fasterxml.jackson.databind.ObjectMapper
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TimestampConditionsTest extends AnyFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: DocumentContext = States.PARSER.parse(
    """
                {
                  "foo": "2012-12-31T23:59:59+08:00",
                  "bar": "2012-12-31T15:59:59Z"
                }
                """
  )


  "TimestampEquals" should "evaluate to false if not equal" in {
    val underTest1 = TimestampConditions.TimestampEquals(JsonPath.compile("$.foo"), "2012-12-31T23:59:58+08:00")
    val underTest2 = TimestampConditions.TimestampEquals(JsonPath.compile("$.bar"), "2013-01-01T00:00:00+08:00")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = TimestampConditions.TimestampEquals(JsonPath.compile("$.foo"), "2012-12-31T23:59:59+08:00")
    val underTest2 = TimestampConditions.TimestampEquals(JsonPath.compile("$.bar"), "2012-12-31T23:59:59+08:00")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "TimestampLessThan" should "evaluate to false if not less than" in {
    val underTest1 = TimestampConditions.TimestampLessThan(JsonPath.compile("$.foo"), "2012-12-31T23:59:59+08:00")
    val underTest2 = TimestampConditions.TimestampLessThan(JsonPath.compile("$.bar"), "2012-12-31T23:59:58+08:00")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if less than" in {
    val underTest1 = TimestampConditions.TimestampLessThan(JsonPath.compile("$.foo"), "2013-01-01T00:00:00+08:00")
    val underTest2 = TimestampConditions.TimestampLessThan(JsonPath.compile("$.bar"), "2013-01-01T00:00:00+08:00")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }

  "TimestampGreaterThan" should "evaluate to false if not greater than" in {
    val underTest1 = TimestampConditions.TimestampGreaterThan(JsonPath.compile("$.foo"), "2012-12-31T23:59:59+08:00")
    val underTest2 = TimestampConditions.TimestampGreaterThan(JsonPath.compile("$.bar"), "2013-01-01T00:00:00+08:00")

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if greater than" in {
    val underTest1 = TimestampConditions.TimestampGreaterThan(JsonPath.compile("$.foo"), "2012-12-31T23:59:58+08:00")
    val underTest2 = TimestampConditions.TimestampGreaterThan(JsonPath.compile("$.bar"), "2012-12-31T23:59:58+08:00")

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
