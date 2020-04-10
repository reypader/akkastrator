package com.akkastrator.state.conditions

import com.akkastrator.state.common.Step
import com.akkastrator.state.conditions.Choices._
import com.akkastrator.state.conditions.LogicalConditions._
import com.fasterxml.jackson.databind.node.NumericNode
import com.jayway.jsonpath.JsonPath

object NumericConditions {

  trait NumericSupport extends Comparison[BigDecimal] with VariableAccess[BigDecimal] {
    override def getActualValue(context: Step#Context, variable: JsonPath): BigDecimal = {
      context.read(variable).asInstanceOf[NumericNode].decimalValue()
    }
  }

  abstract class AbstractNumericEquals(variable: JsonPath, numericEquals: BigDecimal) extends AbstractEqual[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericEquals
  }

  abstract class AbstractNumericLessThan(variable: JsonPath, numericLessThan: BigDecimal) extends AbstractLessThan[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericLessThan
  }

  abstract class AbstractNumericLessThanEquals(variable: JsonPath, numericLessThanEquals: BigDecimal) extends AbstractLessThanEquals[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericLessThanEquals
  }

  abstract class AbstractNumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractGreaterThan[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericGreaterThan
  }

  abstract class AbstractNumericGreaterThanEquals(variable: JsonPath, numericGreaterThanEquals: BigDecimal) extends AbstractGreaterThanEquals[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericGreaterThanEquals
  }

  case class NumericEquals(variable: JsonPath, numericEquals: BigDecimal) extends AbstractNumericEquals(variable, numericEquals)

  case class NumericLessThan(variable: JsonPath, numericLessThan: BigDecimal) extends AbstractNumericLessThan(variable, numericLessThan)

  case class NumericLessThanEquals(variable: JsonPath, numericLessThanEquals: BigDecimal) extends AbstractNumericLessThanEquals(variable, numericLessThanEquals)

  case class NumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractNumericGreaterThan(variable, numericGreaterThan)

  case class NumericGreaterThanEquals(variable: JsonPath, numericGreaterThanEquals: BigDecimal) extends AbstractNumericGreaterThanEquals(variable, numericGreaterThanEquals)

}
