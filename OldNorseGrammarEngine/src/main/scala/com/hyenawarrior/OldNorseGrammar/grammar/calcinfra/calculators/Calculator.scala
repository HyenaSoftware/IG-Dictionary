package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{ShortCoded, Stage}

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait Calculator[D, F] extends ShortCoded {

  def compute(str: D, declension: F, stage: Stage[D, F]): Either[Seq[D], String]

  def reverseCompute(str: D, declension: F, stage: Stage[D, F]): Either[Seq[D], String]
}
