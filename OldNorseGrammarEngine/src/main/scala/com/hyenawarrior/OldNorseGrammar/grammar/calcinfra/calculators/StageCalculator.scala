package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage

/**
  * Created by HyenaWarrior on 2018.10.23..
  */
trait StageCalculator[D, F] extends GenericCalculator[D, F] {

  def compute(stage: Stage[D, F]): Either[Stage[D, F], String]

  def reverseCompute(stage: Stage[D, F]): Either[Stage[D, F], String]
}
