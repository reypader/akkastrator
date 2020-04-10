package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices._
import com.jayway.jsonpath.JsonPath

object LogicalConditions {

  abstract class AbstractEqual[T](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.equals(actualValue)
    }
  }

  abstract class AbstractLessThan[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) < 0
    }
  }

  abstract class AbstractLessThanEquals[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) <= 0
    }
  }

  abstract class AbstractGreaterThan[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) > 0
    }
  }

  abstract class AbstractGreaterThanEquals[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: Step#Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) >= 0
    }
  }

  abstract class AbstractNot extends ChoiceRule {
    def not: ChoiceRule

    def evaluate(context: Step#Context): Boolean = !not.evaluate(context)
  }

  case class Not(not: ChoiceRule) extends AbstractNot

  case class TopNot(not: ChoiceRule, next: String) extends AbstractNot with TopLevelChoice


  abstract class AbstractAnd extends ChoiceRule {
    def and: List[ChoiceRule]

    def evaluate(context: Step#Context): Boolean = and.forall(a => a.evaluate(context))
  }

  case class And(and: List[ChoiceRule]) extends AbstractAnd

  case class TopAnd(and: List[ChoiceRule], next: String) extends AbstractAnd with TopLevelChoice

  abstract class AbstractOr extends ChoiceRule {
    def or: List[ChoiceRule]

    def evaluate(context: Step#Context): Boolean = or.exists(a => a.evaluate(context))
  }

  case class Or(or: List[ChoiceRule]) extends AbstractOr

  case class TopOr(or: List[ChoiceRule], next: String) extends AbstractOr with TopLevelChoice

}
