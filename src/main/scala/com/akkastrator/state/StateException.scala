package com.akkastrator.state

import com.akkastrator.state.common.State
import com.jayway.jsonpath.JsonPath

object StateException {

  case class UnresolvableChoiceException(state: State, context: State#Context) extends Exception
  case class ResultMappingException(cause: Throwable, path: JsonPath, context : State#Context) extends RuntimeException(cause)
}
