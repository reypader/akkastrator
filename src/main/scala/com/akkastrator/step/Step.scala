package com.akkastrator.step

import com.akkastrator.step.Step.Context

import scala.collection.immutable

trait Step {
  def take(context: Context): (Context, Step)
}

object Step {
  type Context = immutable.Map[String, Any]
}
