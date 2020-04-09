package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceState.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.State
import com.akkastrator.state.conditions.LogicalConditions.{AbstractEqual, AbstractGreaterThan, AbstractLessThan}
import com.fasterxml.jackson.databind.node.NumericNode
import com.jayway.jsonpath.JsonPath

object NumericConditions {

  trait NumericSupport extends Comparison[BigDecimal] with VariableAccess[BigDecimal] {
    def expectedValue: NumericNode

    override def comparableValue: BigDecimal = expectedValue.decimalValue()

    override def getActualValue(context: State#Context, variable: JsonPath): BigDecimal = {
      context.read(variable).asInstanceOf[NumericNode].decimalValue()
    }
  }

  abstract class AbstractNumericEquals(variable: JsonPath, numericEquals: NumericNode) extends AbstractEqual[BigDecimal](variable) with NumericSupport {
    override def expectedValue: NumericNode = numericEquals
  }

  abstract class AbstractNumericLessThan(variable: JsonPath, numericLessThan: NumericNode) extends AbstractLessThan[BigDecimal](variable) with NumericSupport {
    override def expectedValue: NumericNode = numericLessThan
  }

  abstract class AbstractNumericGreaterThan(variable: JsonPath, numericGreaterThan: NumericNode) extends AbstractGreaterThan[BigDecimal](variable) with NumericSupport {
    override def expectedValue: NumericNode = numericGreaterThan
  }

  case class NumericEquals(variable: JsonPath, numericEquals: NumericNode) extends AbstractNumericEquals(variable, numericEquals)

  case class NumericLessThan(variable: JsonPath, numericLessThan: NumericNode) extends AbstractNumericLessThan(variable, numericLessThan)

  case class NumericGreaterThan(variable: JsonPath, numericGreaterThan: NumericNode) extends AbstractNumericGreaterThan(variable, numericGreaterThan)


  case class TopNumericEquals(variable: JsonPath, numericEquals: NumericNode, next: String) extends AbstractNumericEquals(variable, numericEquals) with TopLevelChoice

  case class TopNumericLessThan(variable: JsonPath, numericLessThan: NumericNode, next: String) extends AbstractNumericLessThan(variable, numericLessThan) with TopLevelChoice

  case class TopNumericGreaterThan(variable: JsonPath, numericGreaterThan: NumericNode, next: String) extends AbstractNumericGreaterThan(variable, numericGreaterThan) with TopLevelChoice

}
