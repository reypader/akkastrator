package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices._
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath

object StringConditions {

  trait StringSupport extends Comparison[String] with VariableAccess[String] {
    override def getActualValue(context: Step#Context, variable: JsonPath): String = {
      context.read(variable).asInstanceOf[TextNode].textValue()
    }
  }

  abstract class AbstractStringEquals(variable: JsonPath, stringEquals: String) extends AbstractEqual[String](variable) with StringSupport {
    override def comparableValue: String = stringEquals
  }

  abstract class AbstractStringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractLessThan[String](variable) with StringSupport {
    override def comparableValue: String = stringLessThan
  }

  abstract class AbstractStringLessThanEquals(variable: JsonPath, stringLessThanEquals: String) extends AbstractLessThanEquals[String](variable) with StringSupport {
    override def comparableValue: String = stringLessThanEquals
  }

  abstract class AbstractStringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractGreaterThan[String](variable) with StringSupport {
    override def comparableValue: String = stringGreaterThan
  }

  abstract class AbstractStringGreaterThanEquals(variable: JsonPath, stringGreaterThanEquals: String) extends AbstractGreaterThanEquals[String](variable) with StringSupport {
    override def comparableValue: String = stringGreaterThanEquals
  }

  case class StringEquals(variable: JsonPath, stringEquals: String) extends AbstractStringEquals(variable, stringEquals)

  case class StringLessThan(variable: JsonPath, stringLessThan: String) extends AbstractStringLessThan(variable, stringLessThan)

  case class StringLessThanEquals(variable: JsonPath, stringLessThanEquals: String) extends AbstractStringLessThanEquals(variable, stringLessThanEquals)

  case class StringGreaterThan(variable: JsonPath, stringGreaterThan: String) extends AbstractStringGreaterThan(variable, stringGreaterThan)

  case class StringGreaterThanEquals(variable: JsonPath, stringGreaterThanEquals: String) extends AbstractStringGreaterThanEquals(variable, stringGreaterThanEquals)


}
