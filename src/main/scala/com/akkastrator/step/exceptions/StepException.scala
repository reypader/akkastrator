package com.akkastrator.step.exceptions

import com.akkastrator.step.Step

abstract class StepException(step: Step, context: Step.Context) extends Exception

object StepException {

  class DuplicateKeyException(key: String, step: Step, context: Step.Context) extends StepException(step, context)

}
