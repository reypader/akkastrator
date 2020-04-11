package com.akkastrator.state

import com.akkastrator.state.common.States
import com.akkastrator.state.conditions.BooleanConditions.BooleanEquals
import com.akkastrator.state.conditions.LogicalConditions.{And, Not, Or}
import com.jayway.jsonpath.JsonPath
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsError, JsSuccess, Json}

class ParsingTest extends AnyFlatSpec with Matchers with Inside {

  "Simple Hello World Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-simple-hello-world-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("A Hello World example of the Amazon States Language using Pass states")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "Hello"
          states should have size 2
          states.keys should contain("Hello")
          states.keys should contain("World")
          inside(states("Hello")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("World")
              result shouldEqual Some(States.OBJECT_MAPPER.readTree("\"Hello\""))

          }
          inside(states("World")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe true
              next shouldBe None
              result shouldEqual Some(States.OBJECT_MAPPER.readTree("\"World\""))
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }

  "Complex Hello World Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-complex-hello-world-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("A Hello World example demonstrating various state types of the Amazon States Language")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "Pass"
          states should have size 7
          states.keys should contain("Pass")
          states.keys should contain("Hello World example?")
          states.keys should contain("Yes")
          states.keys should contain("No")
          states.keys should contain("Wait 3 sec")
          states.keys should contain("Parallel State")
          states.keys should contain("Hello World")
          inside(states("Pass")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe Some("A Pass state passes its input to its output, without performing work. Pass states are useful when constructing and debugging state machines.")
              end shouldBe false
              next shouldEqual Some("Hello World example?")
              result shouldEqual None

          }
          inside(states("Hello World example?")) {
            case ChoiceState(inputPath, outputPath, comment, choices, default) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe Some("A Choice state adds branching logic to a state machine. Choice rules can implement 16 different comparison operators, and can be combined using And, Or, and Not")
              default shouldBe Some("Yes")
              choices should have size 4
              choices.head.next shouldBe "Yes"
              choices(1).next shouldBe "No"
              choices(2).next shouldBe "No"
              choices(3).next shouldBe "No"
              inside(choices.head.inner) {
                case BooleanEquals(variable, booleanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                  booleanEquals shouldBe true

              }
              inside(choices(1).inner) {
                case Not(not) => inside(not) {
                  case BooleanEquals(variable, booleanEquals) =>
                    variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                    booleanEquals shouldBe true

                }

              }
              inside(choices(2).inner) {
                case And(and) =>
                  and should have size 2
                  inside(and.head) {
                    case BooleanEquals(variable, booleanEquals) =>
                      variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                      booleanEquals shouldBe true

                  }
                  inside(and(1)) {
                    case Not(not) => inside(not) {
                      case BooleanEquals(variable, booleanEquals) =>
                        variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                        booleanEquals shouldBe false

                    }

                  }

              }
              inside(choices(3).inner) {
                case Or(or) =>
                  or should have size 2
                  inside(or.head) {
                    case And(and) =>
                      and should have size 2
                      inside(and.head) {
                        case BooleanEquals(variable, booleanEquals) =>
                          variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                          booleanEquals shouldBe true

                      }
                      inside(and(1)) {
                        case Not(not) => inside(not) {
                          case BooleanEquals(variable, booleanEquals) =>
                            variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                            booleanEquals shouldBe false

                        }

                      }

                  }
                  inside(or(1)) {
                    case BooleanEquals(variable, booleanEquals) =>
                      variable.getPath shouldBe JsonPath.compile("$.IsHelloWorldExample").getPath
                      booleanEquals shouldBe false

                  }


              }
          }
          inside(states("Yes")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("Wait 3 sec")
              result shouldEqual None
          }
          inside(states("No")) {
            case FailState(error, cause, comment) =>
              error shouldBe "FAIL"
              cause shouldBe "Not Hello World"
              comment shouldBe None
          }
          inside(states("Wait 3 sec")) {
            case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe Some("A Wait state delays the state machine from continuing for a specified time.")
              end shouldBe false
              next shouldBe Some("Parallel State")
              seconds shouldBe Some(3)
              timestamp shouldBe None
              secondsPath shouldBe None
              timestampPath shouldBe None
          }
          inside(states("Hello World")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe true
              next shouldEqual None
              result shouldEqual None
          }
          inside(states("Parallel State")) {
            case ParallelState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, branches) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe Some("A Parallel state can be used to create parallel branches of execution in your state machine.")
              end shouldBe false
              next shouldEqual Some("Hello World")
              errorCatch shouldBe None
              errorRetry shouldBe None
              branches should have size 2
              inside(branches.head) {
                case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
                  timeoutSeconds shouldBe None
                  comment shouldBe None
                  version shouldBe "1.0"
                  startAt shouldBe "Hello"
                  states should have size 1
                  states.keys should contain("Hello")
                  inside(states("Hello")) {
                    case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
                      inputPath shouldBe None
                      outputPath shouldBe None
                      resultPath shouldBe None
                      parameters shouldBe None
                      comment shouldBe None
                      end shouldBe true
                      next shouldEqual None
                      result shouldEqual None
                  }
              }
              inside(branches(1)) {
                case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
                  timeoutSeconds shouldBe None
                  comment shouldBe None
                  version shouldBe "1.0"
                  startAt shouldBe "World"
                  states should have size 1
                  states.keys should contain("World")
                  inside(states("World")) {
                    case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
                      inputPath shouldBe None
                      outputPath shouldBe None
                      resultPath shouldBe None
                      parameters shouldBe None
                      comment shouldBe None
                      end shouldBe true
                      next shouldEqual None
                      result shouldEqual None
                  }
              }
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }

  "Wait Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-wait-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("An example of the Amazon States Language using wait states")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "FirstState"
          states should have size 6
          states.keys should contain("FirstState")
          states.keys should contain("wait_using_seconds")
          states.keys should contain("wait_using_timestamp")
          states.keys should contain("wait_using_timestamp_path")
          states.keys should contain("wait_using_seconds_path")
          states.keys should contain("FinalState")
          inside(states("FirstState")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("wait_using_seconds")
              errorCatch shouldBe None
              errorRetry shouldBe None
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"

          }
          inside(states("wait_using_seconds")) {
            case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldBe Some("wait_using_timestamp")
              seconds shouldBe Some(10)
              timestamp shouldBe None
              secondsPath shouldBe None
              timestampPath shouldBe None

          }
          inside(states("wait_using_timestamp")) {
            case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldBe Some("wait_using_timestamp_path")
              seconds shouldBe None
              timestamp shouldBe Some("2015-09-04T01:59:00Z")
              secondsPath shouldBe None
              timestampPath shouldBe None

          }
          inside(states("wait_using_timestamp_path")) {
            case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldBe Some("wait_using_seconds_path")
              seconds shouldBe None
              timestamp shouldBe None
              secondsPath shouldBe None
              timestampPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.expirydate").getPath)

          }
          inside(states("wait_using_seconds_path")) {
            case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldBe Some("FinalState")
              seconds shouldBe None
              timestamp shouldBe None
              secondsPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.expiryseconds").getPath)
              timestampPath shouldBe None

          }
          inside(states("FinalState")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe true
              next shouldEqual None
              errorCatch shouldBe None
              errorRetry shouldBe None
              timeoutSeconds shouldBe 35
              heartBeatSeconds shouldBe Some(5)
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"

          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }
}
