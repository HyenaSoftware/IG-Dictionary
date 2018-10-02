package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait UnitCalculator[D, F] extends Calculator[D, F] {

  override def compute(str: D, declension: F, stage: Stage[D, F]) = Left(Seq(str))

  override def reverseCompute(str: D, declension: F, stage: Stage[D, F]) = Left(Seq(str))

  override def shortCode: String = "UNT"
}