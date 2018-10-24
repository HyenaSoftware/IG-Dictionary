package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.{GenericCalculator, NoOpCalculator}

import scala.collection.Set
import scala.util.hashing.MurmurHash3

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object CalcResult {

  def from[T, F](data: T, declension: F)(implicit noOpCalc: NoOpCalculator[T, F]): CalcResult[T, F] =
    CalcResult[T, F](data, NO_CALC_ON_INPUT, Set(), Set(declension), noOpCalc)
}

case class CalcResult[T, F](data : T, calcDirection : CalcDirection, parentCalcItems : Set[CalcResult[T, F]]
                         , declensions : Set[F], calculator : GenericCalculator[T, F]) extends CalcItem {

  override def hashCode(): Int = {

    val hashCode = MurmurHash3.unorderedHash(Seq(data, calcDirection, declensions, calculator))

    hashCode
  }

  override def equals(that: scala.Any): Boolean = that match {

    case thatCalcResult: CalcResult[T, F] => data == thatCalcResult.data &&
      calcDirection == thatCalcResult.calcDirection &&
      declensions == thatCalcResult.declensions &&
      calculator == thatCalcResult.calculator

    case _ => false
  }

  override def toString: String = toString(true)

  def toString(includeParent: Boolean): String = {

    val prevStr = if(parentCalcItems.isEmpty || !includeParent) "" else parentCalcItems.mkString(" < (", ", ", ")")

    data + ":[" + declensions + "]:" + calcDirection.shortCode + calculator.shortCode + prevStr
  }
}