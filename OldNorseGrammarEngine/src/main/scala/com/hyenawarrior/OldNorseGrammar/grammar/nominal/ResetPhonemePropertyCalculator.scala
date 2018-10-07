package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Default
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{SimpleMorpheme, Word}

/**
  * Created by HyenaWarrior on 2018.10.06..
  */
object ResetPhonemePropertyCalculator extends Calculator[Word, AdjectiveFormType] {

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left(Seq(word))

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]): Either[Seq[Word], String] = {

    val morphemes = word.morphemes

    val newMorphemes = morphemes.map {

      case SimpleMorpheme(phonemes, prop) =>
        val newPhonemes = phonemes.map(ph => ph copyWithPropertyOf Default)
        SimpleMorpheme(newPhonemes, prop)
    }

    Left(Seq(new Word(newMorphemes)))
  }

  override def shortCode: String = "RST"
}
