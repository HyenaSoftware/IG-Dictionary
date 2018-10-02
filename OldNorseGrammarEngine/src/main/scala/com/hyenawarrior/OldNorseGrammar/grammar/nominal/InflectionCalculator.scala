package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.Syncope

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object InflectionCalculator extends Calculator[String, AdjectiveFormType] {

  override def compute(str: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]): Either[Seq[String], String] = {

    val inflection = AdjectiveTraits.inflectionFor(declension)

    val inflectedStr = str + inflection

    Left(Seq(inflectedStr))
  }

  override def reverseCompute(formStrRepr: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]): Either[Seq[String], String] = {

    val inflection = AdjectiveTraits.inflectionWithComparsionFor(declension)

    // remove syncope if needed
    val adjustedInflection = if(formStrRepr endsWith inflection) { inflection } else {

      Syncope adjustSyncopatedInflection inflection
    }

    val formWithoutInflection = morphophonology.stripSuffix(formStrRepr, adjustedInflection)

    if(formWithoutInflection == formStrRepr && inflection.nonEmpty)

      Right(s"$formStrRepr doesn't end with -$inflection at [$declension]")

    else

      Left(Seq(formWithoutInflection))
  }

  override def shortCode: String = "INF"
}