package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.State
import com.akkastrator.state.common.State.State
import com.akkastrator.state.conditions.LogicalConditions.{AbstractEqual, AbstractGreaterThan, AbstractLessThan}
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath

object StringConditions {

  trait StringSupport extends Comparison[String] with VariableAccess[String] {
    override def getActualValue(context: State#Context, variable: JsonPath): String = {
      context.read(variable).asInstanceOf[TextNode].textValue()
    }
  }

  abstract class AbstractStringEquals(variable: JsonPath, stringEquals: String) extends AbstractEqual[String](variable) with StringSupport {
    override def comparableValue: String = stringEquals
  }

  abstract class AbstractStringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractLessThan[String](variable) with StringSupport {
    override def comparableValue: String = stringLessThan
  }

  abstract class AbstractStringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractGreaterThan[String](variable) with StringSupport {
    override def comparableValue: String = stringGreaterThan
  }

  case class StringEquals(variable: JsonPath, stringEquals: String) extends AbstractStringEquals(variable, stringEquals)

  case class StringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractStringLessThan(variable, stringLessThan)

  case class StringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractStringGreaterThan(variable, stringGreaterThan)


  case class TopStringEquals(variable: JsonPath, stringEquals: String, next: String) extends AbstractStringEquals(variable, stringEquals) with TopLevelChoice

  case class TopStringLessThan(variable: JsonPath, stringLessThan: String, next: String) extends AbstractStringLessThan(variable, stringLessThan) with TopLevelChoice

  case class TopStringGreaterThan(variable: JsonPath, stringGreaterThan: String, next: String) extends AbstractStringGreaterThan(variable, stringGreaterThan) with TopLevelChoice

}
