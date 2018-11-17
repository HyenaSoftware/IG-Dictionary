package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.metrics.Metrics.compactnessOf
import com.hyenawarrior.OldNorseGrammar.grammar.nominal.helpers.{CalcResultEx, CalcResultFunc}
/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Context[D, F](stages: List[Stage[D, F]], stem: CalcItem)(implicit impl: CalcResultFunc[D, F]) {

  def qualityIndicator: Int = {

    val stemCorrectness: Int = correctnessOfStem

    val compactness: Int = stem match {
      case cr: CalcResult[D, F] =>

        compactnessOf(cr.sequenceOfPhonemes().values.flatten.toSeq)

        //val countOfSyllables = phonemes.count(ph => ph.isVowel)
      case _ => 0
    }

    stemCorrectness * compactness
  }

  private def correctnessOfStem: Int = stem match {

    case _: CalcError => 0
    case _ => 1
  }
}
