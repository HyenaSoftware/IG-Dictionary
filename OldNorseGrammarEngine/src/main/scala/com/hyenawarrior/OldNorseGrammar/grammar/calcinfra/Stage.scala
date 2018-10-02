package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Stage[D, F](forms: Seq[CalcItem], calculator: Calculator[D, F]) {

  @deprecated("doesn't handle diphtongs")
  def vowelMatrix(implicit vowelsOf: D => Seq[Char]): Seq[Array[Char]] = calcResults.map(str => vowelsOf(str.data).toArray)

  @deprecated("change implementation")
  def countOfSyllables(implicit vowelsOf: D => Seq[Char]): Int = (0 +: vowelMatrix.map(_.length)).max

  def calcResults: Seq[CalcResult[D, F]] = forms.collect { case cr: CalcResult[D, F] => cr }

  def parentCalcResults: Seq[CalcResult[D, F]] = forms
    .collect { case cr: CalcResult[D, F] => cr.parentCalcItems }
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