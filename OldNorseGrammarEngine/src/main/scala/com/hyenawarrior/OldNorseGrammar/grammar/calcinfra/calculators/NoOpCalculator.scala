package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait NoOpCalculator[D, F] extends Calculator[D, F] {

  override def compute(str: D, declension: F, stage: Stage[D, F]) = Left(Seq())

  override def reverseCompute(str: D, declension: F, stage: Stage[D, F]) = Left(Seq())

  override def shortCode: String = "NOP"
}