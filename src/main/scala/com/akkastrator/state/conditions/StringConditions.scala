package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.State
import com.akkastrator.state.conditions.LogicalConditions.{AbstractEqual, AbstractGreaterThan, AbstractLessThan}
import com.fasterxml.jackson.databind.node.TextNode
import com.jayway.jsonpath.JsonPath

object StringConditions {

  trait StringSupport extends Comparison[String] with VariableAccess[String] {
    def expectedValue: TextNode

    override def comparableValue: String = expectedValue.textValue()

    override def getActualValue(context: State#Context, variable: JsonPath): String = {
      context.read(variable).asInstanceOf[TextNode].textValue()
    }
  }

  abstract class AbstractStringEquals(variable: JsonPath, stringEquals: TextNode) extends AbstractEqual[String](variable) with StringSupport {
    override def expectedValue: TextNode = stringEquals
  }

  abstract class AbstractStringLessThan(variable: JsonPath, stringLessThan: TextNode) extends AbstractLessThan[String](variable) with StringSupport {
    override def expectedValue: TextNode = stringLessThan
  }

  abstract class AbstractStringGreaterThan(variable: JsonPath, stringGreaterThan: TextNode) extends AbstractGreaterThan[String](variable) with StringSupport {
    override def expectedValue: TextNode = stringGreaterThan
  }

  case class StringEquals(variable: JsonPath, stringEquals: TextNode) extends AbstractStringEquals(variable, stringEquals)

  case class StringLessThan(variable: JsonPath, stringLessThan: TextNode) extends AbstractStringLessThan(variable, stringLessThan)

  case class StringGreaterThan(variable: JsonPath, stringGreaterThan: TextNode) extends AbstractStringGreaterThan(variable, stringGreaterThan)


  case class TopStringEquals(variable: JsonPath, stringEquals: TextNode, next: String) extends AbstractStringEquals(variable, stringEquals) with TopLevelChoice

  case class TopStringLessThan(variable: JsonPath, stringLessThan: TextNode, next: String) extends AbstractStringLessThan(variable, stringLessThan) with TopLevelChoice

  case class TopStringGreaterThan(variable: JsonPath, stringGreaterThan: TextNode, next: String) extends AbstractStringGreaterThan(variable, stringGreaterThan) with TopLevelChoice

}
