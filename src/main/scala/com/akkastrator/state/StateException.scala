package com.akkastrator.state

import com.akkastrator.state.common.State.State
import com.jayway.jsonpath.JsonPath

object StateException {

  trait ErrorDetails {
    def error: String

    def cause: String
  }

  case class StateFailure(error: String, cause: String) extends Exception with ErrorDetails

  case class StateError(error: String, cause: String, rootCause: Throwable) extends Exception(rootCause) with ErrorDetails

  case class UnresolvableChoiceException(state: State, context: State#Context) extends Exception

  case class ResultMappingException(cause: Throwable, path: JsonPath, context: State#Context) extends RuntimeException(cause)

}
