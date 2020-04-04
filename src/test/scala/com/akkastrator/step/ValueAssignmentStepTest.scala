package com.akkastrator.step

import com.akkastrator.step.exceptions.StepException.DuplicateKeyException
import org.mockito.Mockito.mock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValueAssignmentStepTest extends AnyFlatSpec with Matchers {

  "plain string assignment" should "assign a string as-is" in {
    val fakeNext = mock(classOf[Step])
    val step = ValueAssignmentStep("targetField", "Foo", fakeNext)
    val (context, next) = step.take(Map())

    context shouldEqual Map("targetField" -> "Foo")
    next shouldEqual fakeNext
  }

  it should "throw DuplicateKeyException if the key already exists" in {
    val fakeNext = mock(classOf[Step])
    val step = ValueAssignmentStep("targetField", "Foo", fakeNext)
    assertThrows[DuplicateKeyException] {
      step.take(Map("targetField" -> "Bar"))
    }
  }
}
