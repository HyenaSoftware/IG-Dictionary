package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait UnitCalculator[T] extends Calculator[T] {

  override def compute(str: String, declension: T, stage: Stage[T]) = Left(Seq(str))

  override def reverseCompute(str: String, declension: T, stage: Stage[T]) = Left(Seq(str))

  override def shortCode: String = "UNT"
}