package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.GenericCalculator

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Stage[D, F](forms: Seq[CalcItem], calculator: GenericCalculator[D, F]) {

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