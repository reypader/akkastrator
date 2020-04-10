package com.akkastrator.state

import com.akkastrator.state.States.{Context, TransactionContext}
import com.jayway.jsonpath.JsonPath

object StateException {

  trait ErrorDetails {
    def error: String

    def cause: String
  }

  case class StateFailure(error: String, cause: String) extends Exception with ErrorDetails

  case class StateError(error: String, cause: String, rootCause: Throwable) extends Exception(rootCause) with ErrorDetails

  case class UnresolvableChoiceException(state: States.State, context: TransactionContext) extends Exception

  case class ResultMappingException(cause: Throwable, path: JsonPath, context: Context) extends RuntimeException(cause)

}
