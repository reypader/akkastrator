package com.akkastrator.state

import com.akkastrator.state.common.States.stringListRead
import com.akkastrator.state.common.{CatchError, States}
import com.jayway.jsonpath.JsonPath
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{Reads, _}
import com.akkastrator.state.common.States.jsonPathRead

object ErrorCatch {
  implicit val errorCatchRead: Reads[ErrorCatch] = (
    (JsPath \ "ErrorEquals").read[List[String]] and
      (JsPath \ "Next").read[String] and
      (JsPath \ "ResultPath").readNullable[JsonPath]
    ) (ErrorCatch.apply _)
}

case class ErrorCatch(errorEquals: List[String],
                      next: String,
                      resultPath: Option[JsonPath] = None) {
  if (errorEquals == null || errorEquals.isEmpty) {
    throw new IllegalArgumentException("errorEquals must be specified")
  }
  if (errorEquals.contains(CatchError.ALL) && errorEquals.length != 1) {
    throw new IllegalArgumentException(CatchError.ALL + " must be the only element in errorEquals")
  }
  if (next == null || next.isBlank) {
    throw new IllegalArgumentException("next must not be blank")
  }
  if (resultPath == null) {
    throw new IllegalArgumentException("resultPath must not be null")
  }
}