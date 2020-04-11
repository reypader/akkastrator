package com.akkastrator.state

import com.akkastrator.state.common.States
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsError, JsSuccess, Json}

class ParsingTest extends AnyFlatSpec with Matchers with Inside {

  "Hello World Template" should "produce a complete state machine" in {
    val stateMachine = Json.using[Json.WithDefaultValues].parse(getClass.getClassLoader.getResourceAsStream("aws-simple-hello-world-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          startAt shouldEqual "Hello"
          states should have size 2
          states.keys should contain("Hello")
          states.keys should contain("World")
          inside(states("Hello")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              end shouldBe false
              next shouldBe defined
              next.get shouldEqual "World"
              result shouldBe defined
              result.get shouldEqual States.OBJECT_MAPPER.readTree("\"Hello\"")
            case _ => fail("Unexpected state type")
          }
          inside(states("World")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              end shouldBe true
              next should not be defined
              result shouldBe defined
              result.get shouldEqual States.OBJECT_MAPPER.readTree("\"World\"")
            case _ => fail("Unexpected state type")
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }
}
