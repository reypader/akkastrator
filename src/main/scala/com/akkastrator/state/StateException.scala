package com.akkastrator.state

import com.akkastrator.state.common.State

object StateException {

  case class UnresolvableChoiceException(state: State, context: State#Context) extends Exception

}
