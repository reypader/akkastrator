package com.akkastrator.state

import com.akkastrator.state.common.States.stringListRead
import com.akkastrator.state.common.{CatchError, States}
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}

object ErrorRetry {
  implicit val errorRetryRead: Reads[ErrorRetry] = (
    (JsPath \ "ErrorEquals").read[List[String]] and
      (JsPath \ "IntervalSeconds").read[Int] and
      (JsPath \ "MaxAttempts").read[Int] and
      (JsPath \ "BackOffRate").read[BigDecimal] and
      (JsPath \ "ResultPath").read[String].map(s => JsonPath.compile(s))
    ) (ErrorRetry.apply _)
}


case class ErrorRetry(errorEquals: List[String],
                      intervalSeconds: Int = 1,
                      maxAttempts: Int = 3,
                      backOffRate: BigDecimal = 2,
                      resultPath: JsonPath = States.CONTEXT_ROOT) {
  if (errorEquals == null || errorEquals.isEmpty) {
    throw new IllegalArgumentException("errorEquals must be specified")
  }
  if (errorEquals.contains(CatchError.ALL) && errorEquals.length != 1) {
    throw new IllegalArgumentException(CatchError.ALL + " must be the only element in errorEquals")
  }
  if (intervalSeconds < 1) {
    throw new IllegalArgumentException("intervalSeconds must be positive non-zero")
  }
  if (maxAttempts < 0) {
    throw new IllegalArgumentException("intervalSeconds must be non-negative")
  }
  if (backOffRate < 1) {
    throw new IllegalArgumentException("backOffRate must be greater than or equal to 1")
  }
  if (resultPath == null) {
    throw new IllegalArgumentException("resultPath must not be null")
  }
}