package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Stage[F](forms: Seq[CalcItem], calculator: Calculator[F]) {

  @deprecated("doesn't handle diphtongs")
  def vowelMatrix: Seq[Array[Char]] = calcResults.map(_.data.filter(isVowel).toCharArray)

  @deprecated("change implementation")
  def countOfSyllables: Int = (0 +: vowelMatrix.map(_.length)).max

  def calcResults: Seq[CalcResult[String, F]] = forms.collect { case cr: CalcResult[String, F] => cr }

  def parentCalcResults: Seq[CalcResult[String, F]] = forms
    .collect { case cr: CalcResult[String, F] => cr.parentCalcItems }
    .flatten

  override def toString: String = {

    val formsStr = forms
      .collect {
        case cr: CalcResult[String, F] => s"${cr.data}:${cr.declensions.size}"
        case ce: CalcError => ce.error
      }
      .mkString("[", ", ", "]")
    val calculators = calcResults
      .map(ci => s"${ci.calcDirection.shortCode}${ci.calculator.shortCode}")
      .sorted
      .distinct
      .mkString("[", ", ", "]")

    calculators + formsStr
  }
}