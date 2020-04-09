package com.akkastrator.state

import com.akkastrator.state.common.State
import com.akkastrator.state.common.State.State

case class StateMachine(startAt: String, states: Map[String, State]) {

}
