package com.akkastrator.step.parser

import com.akkastrator.step.Step


object ExpressionToken {
  private def parseHelper(expression: String, context: Step.Context): Any = {
    val nextDot = expression.indexOf(".")
    if (nextDot < 0) {
      return context(expression)
    }
    val key = expression.substring(0, nextDot)
    val newContext = context(key)
    parseHelper(expression.substring(nextDot + 1), newContext.asInstanceOf[Step.Context])
  }

  def apply(expression : String) = new ExpressionToken(expression)
}

class ExpressionToken(expression: String) extends Token {
  def getValue(context: Step.Context): Any = {
    ExpressionToken.parseHelper(expression, context)
  }
}
