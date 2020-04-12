package com.akkastrator.state.conditions

import com.akkastrator.state.common.States
import com.fasterxml.jackson.databind.ObjectMapper
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BooleanConditionsTest extends AnyFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: DocumentContext = States.PARSER.parse(
    """
                {
                  "foo": true,
                  "bar": false
                }
                """
  )


  "BooleanEquals" should "evaluate to false if not equal" in {
    val underTest1 = BooleanConditions.BooleanEquals(JsonPath.compile("$.foo"), booleanEquals = false)
    val underTest2 = BooleanConditions.BooleanEquals(JsonPath.compile("$.bar"), booleanEquals = true)

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = BooleanConditions.BooleanEquals(JsonPath.compile("$.foo"), booleanEquals = true)
    val underTest2 = BooleanConditions.BooleanEquals(JsonPath.compile("$.bar"), booleanEquals = false)

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
