package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.CalcResult
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.NoOpCalculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Morpheme, Phoneme, Word}

/**
  * Created by HyenaWarrior on 2018.09.21..
  */
object helpers {

  implicit object StringNoOpCalculator extends NoOpCalculator[String, AdjectiveFormType]

  implicit object WordNoOpCalculator extends NoOpCalculator[Word, AdjectiveFormType]

  trait CalcResultFunc[T, F] {

    def sequenceOfPhonemes(calcResult: CalcResult[T, F]): Map[Morpheme, Seq[Phoneme]]
  }

  implicit class CalcResultEx[T, F](cr: CalcResult[T, F])(implicit impl: CalcResultFunc[T, F]) {

    def sequenceOfPhonemes(): Map[Morpheme, Seq[Phoneme]] = impl.sequenceOfPhonemes(cr)
  }

  implicit object CalcResultFuncWAdj extends CalcResultFunc[Word, AdjectiveFormType] {

    override def sequenceOfPhonemes(calcResult: CalcResult[Word, AdjectiveFormType]) = {

      calcResult.data.morphemes.map(m => m -> m.phonemes).toMap
    }
  }
}
