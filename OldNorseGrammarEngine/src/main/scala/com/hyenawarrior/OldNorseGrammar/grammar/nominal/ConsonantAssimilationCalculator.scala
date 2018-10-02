package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.ConsonantAssimilation2

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object ConsonantAssimilationCalculator extends Calculator[String, AdjectiveFormType] {

  def compute(str: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]): Either[Seq[String], String] = Left {

    val inflection = AdjectiveTraits.inflectionFor(declension)

    val rs = ConsonantAssimilation2.transform(str, inflection)

    if(rs.nonEmpty) rs else Seq(str)
  }

  def reverseCompute(formStrRepr: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]): Either[Seq[String], String] = Left {

    val inflection = AdjectiveTraits.inflectionFor(declension)

    val rs = ConsonantAssimilation2.reverse(formStrRepr, inflection)

    if(rs.nonEmpty) rs else Seq(formStrRepr)
  }

  override def shortCode: String = "CA"
}
