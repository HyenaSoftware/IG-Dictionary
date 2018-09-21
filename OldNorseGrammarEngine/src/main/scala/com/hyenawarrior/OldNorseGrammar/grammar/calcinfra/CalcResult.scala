package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.{Calculator, NoOpCalculator}

import scala.collection.Set

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object CalcResult {

  def from[T, F](data: T, declension: F)(implicit noOpCalc: NoOpCalculator[F]): CalcResult[T, F] =
    CalcResult[T, F](data, NO_CALC_ON_INPUT, Set(), Set(declension), noOpCalc)
}

case class CalcResult[T, F](data : T, calcDirection : CalcDirection, parentCalcItems : Set[CalcResult[T, F]]
                         , declensions : Set[F], calculator : Calculator[F]) extends CalcItem {

  override def toString: String = toString(true)

  def toString(includeParent: Boolean): String = {

    val prevStr = if(parentCalcItems.isEmpty || !includeParent) "" else parentCalcItems.mkString(" < (", ", ", ")")

    data + ":[" + declensions + "]:" + calcDirection.shortCode + calculator.shortCode + prevStr
  }
}