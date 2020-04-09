package com.akkastrator.state.conditions

import com.akkastrator.state.common.State
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.BooleanNode
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BooleanConditionsTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = State.PARSER.parse(
      """
                {
                  "foo": true,
                  "bar": false
                }
                """
    )
  }

  "BooleanEquals" should "evaluate to false if not equal" in {
    val underTest1 = BooleanConditions.BooleanEquals(JsonPath.compile("$.foo"), om.readTree("""false""").asInstanceOf[BooleanNode])
    val underTest2 = BooleanConditions.BooleanEquals(JsonPath.compile("$.bar"), om.readTree("""true""").asInstanceOf[BooleanNode])

    underTest1.evaluate(data) shouldEqual false
    underTest2.evaluate(data) shouldEqual false
  }

  it should "evaluate to true if equal" in {
    val underTest1 = BooleanConditions.BooleanEquals(JsonPath.compile("$.foo"), om.readTree("""true""").asInstanceOf[BooleanNode])
    val underTest2 = BooleanConditions.BooleanEquals(JsonPath.compile("$.bar"), om.readTree("""false""").asInstanceOf[BooleanNode])

    underTest1.evaluate(data) shouldEqual true
    underTest2.evaluate(data) shouldEqual true
  }
}
