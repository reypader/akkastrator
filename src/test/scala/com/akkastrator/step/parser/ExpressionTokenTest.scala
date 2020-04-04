package com.akkastrator.step.token

import com.akkastrator.step.parser.ExpressionToken
import com.akkastrator.step.parser.TokenException.UndefinedPropertyException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionTokenTest extends AnyFlatSpec with Matchers {

  "malformed expression" should "throw IllegalArgumentException on value starting with dot" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken(".foo")
    }
  }

  it should "throw IllegalArgumentException on value ending with dot" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("foo.")
    }
  }

  it should "throw IllegalArgumentException on value with double dot" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("foo..bar")
    }
  }

  it should "throw IllegalArgumentException on value with unenclosed prefix sequence" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("a'foo'")
    }
  }

  it should "throw IllegalArgumentException on value with unenclosed suffix sequence" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("'foo'a")
    }
  }

  it should "throw IllegalArgumentException on value with 1 end quote" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("foo'")
    }
  }

  it should "throw IllegalArgumentException on value with 1 start quote" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("'foo")
    }
  }

  it should "throw IllegalArgumentException on value with 1 middle quote" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("f'oo")
    }
  }

  it should "throw IllegalArgumentException on value with illegal characters if not literal" in {
    assertThrows[IllegalArgumentException] {
      ExpressionToken("f!oo")
    }
  }

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

  "literal expression" should "assign the value as-is" in {
    val token = ExpressionToken("'foo'")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "foo"
  }

  "literal expression" should "assign the value as-is even with double dot" in {
    val token = ExpressionToken("'fo..o'")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "fo..o"
  }

  "literal expression" should "assign the value as-is even with special characters" in {
    val token = ExpressionToken("'fo!o'")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "fo!o"
  }


  "literal expression" should "assign the value as-is even with inserted quote" in {
    val token = ExpressionToken("'fo'o'")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "fo'o"
  }

  "literal expression" should "assign the value as-is even with two inserted quotes" in {
    val token = ExpressionToken("'f'o'o'")

    val value = token.getValue(Map("foo" -> "bar"))

    value shouldEqual "f'o'o"
  }


}
