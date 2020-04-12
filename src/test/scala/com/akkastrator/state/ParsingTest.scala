package com.akkastrator.state

import java.time.OffsetDateTime

import com.akkastrator.state.common.States
import com.akkastrator.state.conditions.BooleanConditions.BooleanEquals
import com.akkastrator.state.conditions.LogicalConditions.{And, Not, Or}
import com.akkastrator.state.conditions.NumericConditions.{NumericEquals, NumericGreaterThan, NumericGreaterThanEquals, NumericLessThan, NumericLessThanEquals}
import com.akkastrator.state.conditions.StringConditions.{StringEquals, StringGreaterThan, StringGreaterThanEquals, StringLessThan, StringLessThanEquals}
import com.akkastrator.state.conditions.TimestampConditions.{TimestampEquals, TimestampGreaterThan, TimestampGreaterThanEquals, TimestampLessThan, TimestampLessThanEquals}
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
              timestamp shouldBe Some(OffsetDateTime.parse("2015-09-04T01:59:00Z"))
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

  "Choice Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-choice-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("An example of the Amazon States Language using a choice state.")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "FirstState"
          states should have size 6
          states.keys should contain("FirstState")
          states.keys should contain("ChoiceState")
          states.keys should contain("FirstMatchState")
          states.keys should contain("SecondMatchState")
          states.keys should contain("DefaultState")
          states.keys should contain("NextState")
          inside(states("FirstState")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("ChoiceState")
              errorCatch shouldBe None
              errorRetry shouldBe None
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"
          }
          inside(states("FirstMatchState")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("NextState")
              errorCatch shouldBe None
              errorRetry shouldBe None
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:OnFirstMatch"
          }
          inside(states("SecondMatchState")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("NextState")
              errorCatch shouldBe None
              errorRetry shouldBe None
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:OnSecondMatch"
          }
          inside(states("DefaultState")) {
            case FailState(error, cause, comment) =>
              error shouldBe "DefaultStateError"
              cause shouldBe "No Matches!"
              comment shouldBe None
          }
          inside(states("NextState")) {
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
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"
          }
          inside(states("ChoiceState")) {
            case ChoiceState(inputPath, outputPath, comment, choices, default) =>
              inputPath shouldBe None
              outputPath shouldBe None
              comment shouldBe None
              default shouldBe Some("DefaultState")
              choices should have size 15
              choices.head.next shouldBe "FirstMatchState"
              choices(1).next shouldBe "SecondMatchState"
              choices(2).next shouldBe "SecondMatchState"
              choices(3).next shouldBe "SecondMatchState"
              choices(4).next shouldBe "SecondMatchState"
              choices(5).next shouldBe "FirstMatchState"
              choices(6).next shouldBe "SecondMatchState"
              choices(7).next shouldBe "SecondMatchState"
              choices(8).next shouldBe "SecondMatchState"
              choices(9).next shouldBe "SecondMatchState"
              choices(10).next shouldBe "FirstMatchState"
              choices(11).next shouldBe "SecondMatchState"
              choices(12).next shouldBe "SecondMatchState"
              choices(13).next shouldBe "SecondMatchState"
              choices(14).next shouldBe "FirstMatchState"
              inside(choices.head.inner) {
                case NumericEquals(variable, numericEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  numericEquals shouldBe 1
              }
              inside(choices(1).inner) {
                case NumericLessThan(variable, numericLessThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  numericLessThan shouldBe 2
              }
              inside(choices(2).inner) {
                case NumericLessThanEquals(variable, numericLessThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  numericLessThanEquals shouldBe 3
              }
              inside(choices(3).inner) {
                case NumericGreaterThan(variable, numericGreaterThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  numericGreaterThan shouldBe 2
              }
              inside(choices(4).inner) {
                case NumericGreaterThanEquals(variable, numericGreaterThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  numericGreaterThanEquals shouldBe 3
              }

              inside(choices(5).inner) {
                case StringEquals(variable, stringEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  stringEquals shouldBe "1"
              }
              inside(choices(6).inner) {
                case StringLessThan(variable, stringLessThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  stringLessThan shouldBe "2"
              }
              inside(choices(7).inner) {
                case StringLessThanEquals(variable, stringLessThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  stringLessThanEquals shouldBe "3"
              }
              inside(choices(8).inner) {
                case StringGreaterThan(variable, stringGreaterThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  stringGreaterThan shouldBe "2"
              }
              inside(choices(9).inner) {
                case StringGreaterThanEquals(variable, stringGreaterThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  stringGreaterThanEquals shouldBe "3"
              }

              inside(choices(10).inner) {
                case TimestampEquals(variable, timestampEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  timestampEquals shouldBe "1"
              }
              inside(choices(11).inner) {
                case TimestampLessThan(variable, timestampLessThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  timestampLessThan shouldBe "2"
              }
              inside(choices(12).inner) {
                case TimestampLessThanEquals(variable, timestampLessThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  timestampLessThanEquals shouldBe "3"
              }
              inside(choices(13).inner) {
                case TimestampGreaterThan(variable, timestampGreaterThan) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  timestampGreaterThan shouldBe "2"
              }
              inside(choices(14).inner) {
                case TimestampGreaterThanEquals(variable, timestampGreaterThanEquals) =>
                  variable.getPath shouldBe JsonPath.compile("$.foo").getPath
                  timestampGreaterThanEquals shouldBe "3"
              }
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }

  "Map Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-map-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("An example of the Amazon States Language using a map state to process elements of an array with a max concurrency of 2.")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "Map"
          states should have size 2
          states.keys should contain("Map")
          states.keys should contain("Final State")
          inside(states("Map")) {
            case MapState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, iterator, itemsPath, maxConcurrency) =>
              itemsPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.array").getPath)
              outputPath shouldBe None
              inputPath shouldBe None
              resultPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.array").getPath)
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("Final State")
              errorCatch shouldBe None
              errorRetry shouldBe None
              maxConcurrency shouldEqual 2
              inside(iterator) {
                case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
                  timeoutSeconds shouldBe None
                  version shouldBe "1.0"
                  comment shouldBe None
                  startAt shouldBe "Pass"
                  states should have size 1
                  states.keys should contain("Pass")
                  inside(states("Pass")) {
                    case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
                      inputPath shouldBe None
                      resultPath shouldBe None
                      outputPath shouldBe None
                      end shouldBe true
                      next shouldBe None
                      parameters shouldBe None
                      comment shouldBe None
                      result shouldBe Some(States.OBJECT_MAPPER.readTree("\"Done!\""))
                  }
              }
          }
          inside(states("Final State")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              resultPath shouldBe None
              outputPath shouldBe None
              end shouldBe true
              next shouldBe None
              parameters shouldBe None
              comment shouldBe None
              result shouldBe None
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }

  "Parallel Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-parallel-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("An example of the Amazon States Language using a parallel state to execute two branches at the same time.")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "Parallel"
          states should have size 2
          states.keys should contain("Parallel")
          states.keys should contain("Final State")
          inside(states("Parallel")) {
            case ParallelState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, branches) =>
              outputPath shouldBe None
              inputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe false
              next shouldEqual Some("Final State")
              errorCatch shouldBe None
              errorRetry shouldBe None
              branches should have size 2
              inside(branches.head) {
                case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
                  timeoutSeconds shouldBe None
                  version shouldBe "1.0"
                  comment shouldBe None
                  startAt shouldBe "Wait 20s"
                  states should have size 1
                  states.keys should contain("Wait 20s")
                  inside(states("Wait 20s")) {
                    case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
                      inputPath shouldBe None
                      resultPath shouldBe None
                      outputPath shouldBe None
                      end shouldBe true
                      next shouldBe None
                      parameters shouldBe None
                      comment shouldBe None
                      seconds shouldBe Some(20)
                      timestamp shouldBe None
                      secondsPath shouldBe None
                      timestampPath shouldBe None
                  }
              }
              inside(branches(1)) {
                case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
                  timeoutSeconds shouldBe None
                  version shouldBe "1.0"
                  comment shouldBe None
                  startAt shouldBe "Pass"
                  states should have size 2
                  states.keys should contain("Pass")
                  states.keys should contain("Wait 10s")
                  inside(states("Pass")) {
                    case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
                      inputPath shouldBe None
                      resultPath shouldBe None
                      outputPath shouldBe None
                      end shouldBe false
                      next shouldBe Some("Wait 10s")
                      parameters shouldBe None
                      comment shouldBe None
                      result shouldBe None

                  }
                  inside(states("Wait 10s")) {
                    case WaitState(inputPath, outputPath, end, next, comment, seconds, timestamp, secondsPath, timestampPath) =>
                      inputPath shouldBe None
                      resultPath shouldBe None
                      outputPath shouldBe None
                      end shouldBe true
                      next shouldBe None
                      parameters shouldBe None
                      comment shouldBe None
                      seconds shouldBe Some(10)
                      timestamp shouldBe None
                      secondsPath shouldBe None
                      timestampPath shouldBe None
                  }
              }

              inside(states("Final State")) {
                case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
                  inputPath shouldBe None
                  resultPath shouldBe None
                  outputPath shouldBe None
                  end shouldBe true
                  next shouldBe None
                  parameters shouldBe None
                  comment shouldBe None
                  result shouldBe None
              }
          }
      }
      case JsError(e) => fail("Errors: " + JsError.toJson(e).toString())
    }
  }

  "Retry Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-retry-failure-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("A Retry example of the Amazon States Language using an AWS Lambda Function")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "HelloWorld"
          states should have size 1
          states.keys should contain("HelloWorld")
          inside(states("HelloWorld")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry, errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe true
              next shouldEqual None
              errorCatch shouldBe None
              errorRetry shouldBe defined
              val retries = errorRetry.get
              retries should have size 3
              inside(retries.head) {
                case ErrorRetry(errorEquals, intervalSeconds, maxAttempts, backOffRate, resultPath) =>
                  errorEquals shouldBe List("CustomError")
                  intervalSeconds shouldBe 1
                  maxAttempts shouldBe 2
                  backOffRate shouldBe BigDecimal(2.1)
                  resultPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.bar").getPath)
              }
              inside(retries(1)) { case ErrorRetry(errorEquals, intervalSeconds, maxAttempts, backOffRate, resultPath) =>
                errorEquals shouldBe List("States.TaskFailed")
                intervalSeconds shouldBe 30
                maxAttempts shouldBe 2
                backOffRate shouldBe BigDecimal(2)
                resultPath shouldBe None
              }
              inside(retries(2)) { case ErrorRetry(errorEquals, intervalSeconds, maxAttempts, backOffRate, resultPath) =>
                errorEquals shouldBe List("States.ALL")
                intervalSeconds shouldBe 5
                maxAttempts shouldBe 5
                backOffRate shouldBe BigDecimal(2.0)
                resultPath shouldBe None
              }
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"
          }
      }
    }
  }

  "Catch Template" should "produce a complete state machine" in {
    val stateMachine = Json.parse(getClass.getClassLoader.getResourceAsStream("aws-catch-failure-template.json")).validate[StateMachine]

    stateMachine match {
      case sm: JsSuccess[StateMachine] => inside(sm.get) {
        case StateMachine(startAt, states, timeoutSeconds, version, comment) =>
          timeoutSeconds shouldBe None
          comment shouldBe Some("A Catch example of the Amazon States Language using an AWS Lambda Function")
          version shouldBe StateMachine.DEFAULT_VERSION
          startAt shouldEqual "HelloWorld"
          states should have size 4
          states.keys should contain("HelloWorld")
          states.keys should contain("CustomErrorFallback")
          states.keys should contain("ReservedTypeFallback")
          states.keys should contain("CatchAllFallback")
          inside(states("HelloWorld")) {
            case TaskState(inputPath, resultPath, outputPath, end, next, parameters, comment, errorRetry,errorCatch, resource, timeoutSeconds, heartBeatSeconds) =>
              inputPath shouldBe None
              outputPath shouldBe None
              resultPath shouldBe None
              parameters shouldBe None
              comment shouldBe None
              end shouldBe true
              next shouldEqual None
              errorRetry shouldBe None
              errorCatch shouldBe defined
              val catches = errorCatch.get
              catches should have size 3
              inside(catches.head) {
                case ErrorCatch(errorEquals, next, resultPath) =>
                  errorEquals shouldBe List("CustomError")
                  next shouldBe "CustomErrorFallback"
                  resultPath.map(_.getPath) shouldBe Some(JsonPath.compile("$.bar").getPath)
              }
              inside(catches(1)) { case ErrorCatch(errorEquals, next, resultPath) =>
                errorEquals shouldBe List("States.TaskFailed")
                next shouldBe "ReservedTypeFallback"
                resultPath shouldBe None
              }
              inside(catches(2)) { case ErrorCatch(errorEquals, next, resultPath) =>
                errorEquals shouldBe List("States.ALL")
                next shouldBe "CatchAllFallback"
                resultPath shouldBe None
              }
              timeoutSeconds shouldBe 60
              heartBeatSeconds shouldBe None
              resource shouldBe "arn:aws:lambda:REGION:ACCOUNT_ID:function:FUNCTION_NAME"
          }
          inside(states("CustomErrorFallback")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              resultPath shouldBe None
              outputPath shouldBe None
              end shouldBe true
              next shouldBe None
              parameters shouldBe None
              result shouldBe Some(States.OBJECT_MAPPER.readTree("\"This is a fallback from a custom lambda function exception\""))
              comment shouldBe None
          }
          inside(states("ReservedTypeFallback")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              resultPath shouldBe None
              outputPath shouldBe None
              end shouldBe true
              next shouldBe None
              parameters shouldBe None
              result shouldBe Some(States.OBJECT_MAPPER.readTree("\"This is a fallback from a reserved error code\""))
              comment shouldBe None
          }
          inside(states("CatchAllFallback")) {
            case PassState(inputPath, resultPath, outputPath, end, next, parameters, comment, result) =>
              inputPath shouldBe None
              resultPath shouldBe None
              outputPath shouldBe None
              end shouldBe true
              next shouldBe None
              parameters shouldBe None
              result shouldBe Some(States.OBJECT_MAPPER.readTree("\"This is a fallback from a reserved error code\""))
              comment shouldBe None
          }
      }
    }
  }
}



