package com.akkastrator.step

import com.akkastrator.step.exceptions.StepException.DuplicateKeyException

case class ValueAssignmentStep(targetFieldName: String, sourceExpression: String, nextStep: Step) extends Step {
  override def take(context: Step.Context): (Step.Context, Step) = {
    if (context.contains(targetFieldName)) {
      throw new DuplicateKeyException(targetFieldName, this, context)
    } else {
      val newContext = context + (targetFieldName -> sourceExpression)
      (newContext, nextStep)
    }
  }
}
