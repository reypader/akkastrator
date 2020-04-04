package com.akkastrator.step.token

import com.akkastrator.step.exceptions.StepException.DuplicateKeyException
import com.akkastrator.step.parser.ExpressionToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionTokenTest extends AnyFlatSpec with Matchers {
  "single-layer expression string" should "extract the value from the context" in {
    val token = ExpressionToken("foo")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "bar"
  }

  it should "throw UndefinedPropertyException if the property does not exist" in {
    val token = ExpressionToken("foo")
    assertThrows[UndefinedPropertyException] {
      token.getValue(Map())
    }
  }

  "double-layer expression string" should "extract the value from the context" in {
    val token = ExpressionToken("foo.bar")

    val value = token.getValue(Map("foo" -> Map("bar" -> "baz")))

    value shouldEqual "baz"
  }
}
