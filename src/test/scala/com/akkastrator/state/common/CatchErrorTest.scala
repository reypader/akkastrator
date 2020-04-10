package com.akkastrator.state.common

import java.util.UUID

import com.akkastrator.state.StateException
import com.akkastrator.state.common.States.{Decision, TransactionContext}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.JsonPath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CatchErrorTest extends AnyFlatSpec with Matchers {
  val om: ObjectMapper = new ObjectMapper()
  val data: TransactionContext = TransactionContext(UUID.randomUUID(), States.PARSER.parse(
    """
      {
        "foo": "bar"
      }
      """
  ), "test")

  object Fake extends CatchError {
    override def errorCatch: List[Fake.Catcher] = List(Catcher(List("A"), "nextA"),
      Catcher(List("B"), "nextB", JsonPath.compile("$.err.errr")),
      Catcher(List("C", "D"), "nextC"),
      Catcher(List(CatchError.ALL), "nextX"))
  }


  "Catch" should "return next normally if no resultPath is provided" in {
    val result = Fake.handle(data, StateException.StateFailure("A", "1"))
    val Decision(context) = result.get
    context.currentState shouldEqual "nextA"
    context.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
        {
          "error" : "A",
          "cause" : "1"
        }
        """)
  }

  it should "return next and inject error if resultPath is provided" in {
    val result = Fake.handle(data, StateException.StateFailure("B", "2"))

    val Decision(context) = result.get
    context.currentState shouldEqual "nextB"
    context.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
         {
        "foo": "bar",
        "err" : {
          "errr": {
           "error" : "B",
            "cause" : "2"
          }
        }
      }
        """)

  }

  it should "should be able to search the list of errors" in {
    val result = Fake.handle(data, StateException.StateFailure("D", "3"))
    val Decision(context) = result.get
    context.currentState shouldEqual "nextC"
    context.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
         {
          "error" : "D",
          "cause" : "3"
        }
        """)
  }


  it should "should be able to catch all" in {
    val result = Fake.handle(data, StateException.StateFailure("Z", "9"))
    val Decision(context) = result.get
    context.currentState shouldEqual "nextX"
    context.data.read[JsonNode](States.CONTEXT_ROOT) shouldEqual om.readTree(
      """
         {
          "error" : "Z",
          "cause" : "9"
        }
        """)

  }
}
