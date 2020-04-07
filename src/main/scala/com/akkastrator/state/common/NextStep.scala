package com.akkastrator.state.common

trait NextStep {
  def next: Option[String]
}
