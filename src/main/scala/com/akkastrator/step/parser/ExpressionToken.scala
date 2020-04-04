package com.akkastrator.step.parser

import com.akkastrator.step.Step
import com.akkastrator.step.parser.TokenException.{NonLiteralValueException, UndefinedPropertyException}


case class ExpressionToken(expression: String) extends Token {
  val isLiteral: Boolean = expression.startsWith("'") && expression.endsWith("'")

  if (!isLiteral && !expression.matches("\\$\\.[-a-zA-Z0-9_]+(\\.[-a-zA-Z0-9_]+)*")) {
    throw new IllegalArgumentException(expression)
  }


  val literalValue: Option[String] = if (isLiteral) Some(expression.substring(1, expression.length - 1)) else None

  override def getValue(context: Step.Context): String = literalValue.getOrElse(parseHelper(expression.substring(2), context).toString)

  private def testAnyVal[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null

  private def parseHelper(expression: String, context: Step.Context): Any = {
    var key: String = expression
    val nextDot = expression.indexOf(".")
    val hasSubPath = nextDot > 0
    if (hasSubPath) {
      key = expression.substring(0, nextDot)
    }

    if (!context.contains(key)) {
      throw new TokenException.UndefinedPropertyException(this, context)
    }

    val currentValue = context(key)
    if (hasSubPath) {
      parseHelper(expression.substring(nextDot + 1), currentValue.asInstanceOf[Step.Context])
    } else if (currentValue.isInstanceOf[String] || testAnyVal(currentValue)) {
      currentValue.toString
    } else {
      throw new NonLiteralValueException(this, context)
    }
  }
}
