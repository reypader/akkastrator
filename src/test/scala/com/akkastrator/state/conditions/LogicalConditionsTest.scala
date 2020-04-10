package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceStep.ChoiceRule
import com.akkastrator.state.common.Step
import com.fasterxml.jackson.databind.ObjectMapper
import com.jayway.jsonpath.DocumentContext
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogicalConditionsTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  val om: ObjectMapper = new ObjectMapper()
  var data: DocumentContext = Step.PARSER.parse(
    """
                {
                  "foo": "bar"
                }
                """
  )


  "Not" should "should negate" in {
    val mockCondition = mock(classOf[ChoiceRule])
    when(mockCondition.evaluate(data)).thenReturn(true)
    val underTest = LogicalConditions.Not(mockCondition)

    underTest.evaluate(data) shouldEqual false
  }

  "And" should "return true if all is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(true)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(true)
    val underTest = LogicalConditions.And(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual true
  }

  it should "return false if not all is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(true)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(false)
    val underTest = LogicalConditions.And(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual false
  }

  it should "return false if none is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(false)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(false)
    val underTest = LogicalConditions.And(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual false
  }


  "Or" should "return true if all is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(true)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(true)
    val underTest = LogicalConditions.Or(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual true
  }

  it should "return false if not all is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(true)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(false)
    val underTest = LogicalConditions.Or(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual true
  }

  it should "return false if none is true" in {
    val mockCondition1 = mock(classOf[ChoiceRule])
    when(mockCondition1.evaluate(data)).thenReturn(false)

    val mockCondition2 = mock(classOf[ChoiceRule])
    when(mockCondition2.evaluate(data)).thenReturn(false)
    val underTest = LogicalConditions.Or(List(mockCondition1, mockCondition2))

    underTest.evaluate(data) shouldEqual false
  }
}
