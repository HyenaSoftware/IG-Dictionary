package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.{NoOpCalculator, UnitCalculator}

/**
  * Created by HyenaWarrior on 2018.09.21..
  */
object helpers {

  implicit object NoOpCalculator extends NoOpCalculator[AdjectiveFormType]
  implicit object UnitCalculator extends UnitCalculator[AdjectiveFormType]
}
