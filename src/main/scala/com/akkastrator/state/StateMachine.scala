package com.akkastrator.state

import com.akkastrator.state.common.Step
import com.akkastrator.state.common.Step.Step

case class StateMachine(startAt: String, states: Map[String, Step]) {

}
