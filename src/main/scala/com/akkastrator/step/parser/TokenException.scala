package com.akkastrator.step.parser

import com.akkastrator.step.Step

abstract class TokenException(token: Token, context: Step.Context) extends Exception


object TokenException {

  class UndefinedPropertyException(token: Token, context: Step.Context) extends TokenException(token, context)

}