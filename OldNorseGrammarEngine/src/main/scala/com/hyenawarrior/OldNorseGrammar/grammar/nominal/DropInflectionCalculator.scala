package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Morpheme, MorphemeProperty, Word}

/**
  * Created by HyenaWarrior on 2018.10.03..
  */
object DropInflectionCalculator extends Calculator[Word, AdjectiveFormType] {

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left {

    val inflection = Morpheme(inflectionWithComparsionFor(declension), MorphemeProperty.Suffix)

    Seq(word + inflection)
  }

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left {

    Seq(Word(word.selectMorpheme(MorphemeProperty.Stem).toSeq))
  }

  override def shortCode: String = "INF"
}
