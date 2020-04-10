package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{ChoiceRule, Comparison, VariableAccess}
import com.akkastrator.state.States
import com.jayway.jsonpath.JsonPath
import play.api.libs.json.{Reads, _}

object LogicalConditions {

  abstract class AbstractEqual[T](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      comparableValue.equals(actualValue)
    }
  }

  abstract class AbstractLessThan[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) < 0
    }
  }

  abstract class AbstractLessThanEquals[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) <= 0
    }
  }

  abstract class AbstractGreaterThan[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) > 0
    }
  }

  abstract class AbstractGreaterThanEquals[T <: Comparable[T]](variable: JsonPath) extends ChoiceRule with Comparison[T] with VariableAccess[T] {
    def evaluate(context: States.Context): Boolean = {
      val actualValue = getActualValue(context, variable)
      actualValue.compareTo(comparableValue) >= 0
    }
  }

  abstract class AbstractNot extends ChoiceRule {
    def not: ChoiceRule

    def evaluate(context: States.Context): Boolean = !not.evaluate(context)
  }

  abstract class AbstractAnd extends ChoiceRule {
    def and: List[ChoiceRule]

    def evaluate(context: States.Context): Boolean = and.forall(a => a.evaluate(context))
  }

  abstract class AbstractOr extends ChoiceRule {
    def or: List[ChoiceRule]

    def evaluate(context: States.Context): Boolean = or.exists(a => a.evaluate(context))
  }

  case class Not(not: ChoiceRule) extends AbstractNot

  case class And(and: List[ChoiceRule]) extends AbstractAnd

  case class Or(or: List[ChoiceRule]) extends AbstractOr

  implicit val andRead: Reads[And] = Json.reads[And]
  implicit val orRead: Reads[Or] = Json.reads[Or]
  implicit val notRead: Reads[Not] = Json.reads[Not]
}
