package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.{NoOpCalculator, UnitCalculator}

/**
  * Created by HyenaWarrior on 2018.09.21..
  */
object helpers {

  implicit object StringNoOpCalculator extends NoOpCalculator[String, AdjectiveFormType]
  implicit object StringUnitCalculator extends UnitCalculator[String, AdjectiveFormType]
}