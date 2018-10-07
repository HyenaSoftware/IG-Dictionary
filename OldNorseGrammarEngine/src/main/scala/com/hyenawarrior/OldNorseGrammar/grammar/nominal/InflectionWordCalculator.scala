package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.Suffix
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{SimpleMorpheme, Word}

/**
  * Created by HyenaWarrior on 2018.09.30..
  */
object InflectionWordCalculator extends Calculator[phonology.Word, AdjectiveFormType] {

  override def reverseCompute(str: phonology.Word, declension: AdjectiveFormType, stage: Stage[phonology.Word, AdjectiveFormType]) = Left {

    val inflection = inflectionWithComparsionFor(declension)

    val wordForm = str splitAt (-1 - inflection.length)

    Seq(wordForm.transformMorphemes {

      case (mp, 1) => SimpleMorpheme(mp.phonemes, Suffix)
    })
  }

  override def compute(str: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left(Seq(str))

  override def shortCode: String = "INF"
}
