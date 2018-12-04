package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.metrics.Metrics.compactnessOf
import com.hyenawarrior.OldNorseGrammar.grammar.nominal.helpers.{CalcResultEx, CalcResultFunc}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Semivowel, Vowel2}
/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Context[D, F](stages: List[Stage[D, F]], stem: CalcItem)(implicit impl: CalcResultFunc[D, F]) {

  def qualityIndicator: Int = {

    val stemCorrectness: Int = correctnessOfStem

    val (compactness, lengthInd) = stem match {
      case cr: CalcResult[D, F] =>

        val phs = cr.sequenceOfPhonemes().values.flatten.toSeq
        compactnessOf(phs) -> ((15 - phs.length) * 1000)

      case _ => 0 -> 0
    }

    stemCorrectness * (compactness + lengthInd)
  }

  private def correctnessOfStem: Int = stem match {

    case _: CalcError => 0
    case cr: CalcResult[D, F] => {

      val phs = cr.sequenceOfPhonemes().values.flatten
      val vowels = phs.collect { case ph: Vowel2 => ph }
      val semivowels = phs.collect { case ph: Semivowel => ph }

      val hasMutated = vowels.exists(_.isFrontMutated)
      val hasSemivowel = semivowels.nonEmpty

      if(hasMutated && hasSemivowel) 2 else 1
    }
  }
}
