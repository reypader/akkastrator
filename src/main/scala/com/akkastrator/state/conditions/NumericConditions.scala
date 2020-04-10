package com.akkastrator.state.conditions

import com.akkastrator.state.ChoiceStep.{Comparison, TopLevelChoice, VariableAccess}
import com.akkastrator.state.common.Step
import com.akkastrator.state.common.Step.Step
import com.akkastrator.state.conditions.LogicalConditions.{AbstractEqual, AbstractGreaterThan, AbstractLessThan}
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

  abstract class AbstractNumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractGreaterThan[BigDecimal](variable) with NumericSupport {
    override def comparableValue: BigDecimal = numericGreaterThan
  }

  case class NumericEquals(variable: JsonPath, numericEquals: BigDecimal) extends AbstractNumericEquals(variable, numericEquals)

  case class NumericLessThan(variable: JsonPath, numericLessThan: BigDecimal) extends AbstractNumericLessThan(variable, numericLessThan)

  case class NumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal) extends AbstractNumericGreaterThan(variable, numericGreaterThan)


  case class TopNumericEquals(variable: JsonPath, numericEquals: BigDecimal, next: String) extends AbstractNumericEquals(variable, numericEquals) with TopLevelChoice

  case class TopNumericLessThan(variable: JsonPath, numericLessThan: BigDecimal, next: String) extends AbstractNumericLessThan(variable, numericLessThan) with TopLevelChoice

  case class TopNumericGreaterThan(variable: JsonPath, numericGreaterThan: BigDecimal, next: String) extends AbstractNumericGreaterThan(variable, numericGreaterThan) with TopLevelChoice

}
