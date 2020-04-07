package com.akkastrator.state.common

abstract class FreeState(stateType: String, next: Option[String] = None, end: Boolean = false) extends State(stateType) {
  if (end && next.isDefined) {
    throw new IllegalArgumentException("`next` step must not be defined if `end` is true")
  }
  if (!end && next.isEmpty) {
    throw new IllegalArgumentException("`next` step must be defined if `end` is false")
  }
}