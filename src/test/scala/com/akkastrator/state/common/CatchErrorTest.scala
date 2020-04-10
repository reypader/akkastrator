package com.akkastrator.state.common

import com.akkastrator.state.StateException
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.jayway.jsonpath.{DocumentContext, JsonPath}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CatchErrorTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = _

  override def beforeEach(): Unit = {
    data = Step.PARSER.parse(
      """
      {
        "foo": "bar"
      }
      """
    )
  }

  object Fake extends CatchError {
    override def catchError: List[Fake.Catcher] = List(Catcher(List("A"), "nextA"),
      Catcher(List("B"), "nextB", JsonPath.compile("$.err.errr")),
      Catcher(List("C", "D"), "nextC"),
      Catcher(List(CatchError.ALL), "nextX"))
  }


  "Catch" should "return next normally if no resultPath is provided" in {
    val result = Fake.handle(data, StateException.StateFailure("A", "1"))
    val (next, context) = result.get
    (next, context.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode]) shouldEqual("nextA", om.readTree(
      """
        {
          "error" : "A",
          "cause" : "1"
        }
        """))
  }

  it should "return next and inject error if resultPath is provided" in {
    val result = Fake.handle(data, StateException.StateFailure("B", "2"))

    val (next, context) = result.get
    (next, context.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode]) shouldEqual("nextB", om.readTree(
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

        """))
  }

  it should "should be able to search the list of errors" in {
    val result = Fake.handle(data, StateException.StateFailure("D", "3"))

    val (next, context) = result.get
    (next, context.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode]) shouldEqual("nextC", om.readTree(
      """
        {
          "error" : "D",
          "cause" : "3"
        }
        """))
  }


  it should "should be able to catch all" in {
    val result = Fake.handle(data, StateException.StateFailure("Z", "9"))

    val (next, context) = result.get
    (next, context.read(Step.CONTEXT_ROOT).asInstanceOf[JsonNode]) shouldEqual("nextX", om.readTree(
      """
        {
          "error" : "Z",
          "cause" : "9"
        }
        """))
  }
}
