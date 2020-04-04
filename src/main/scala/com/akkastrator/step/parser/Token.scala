package com.akkastrator.step.parser

import com.akkastrator.step.Step

trait Token {
  def getValue(context: Step.Context): String
}
