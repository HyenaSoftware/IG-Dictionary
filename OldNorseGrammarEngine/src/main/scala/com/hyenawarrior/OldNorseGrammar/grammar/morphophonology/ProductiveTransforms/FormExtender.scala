package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.StageCalculator
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{CALC_DOWN_FROM_STEM, CalcResult, Stage}

import scala.collection.Set

/**
  * Created by HyenaWarrior on 2018.11.17..
  */
case class FormExtender[D, F](missingDeclensions: Set[F]) extends StageCalculator[D, F] {

  override def compute(stage: Stage[D, F]): Either[Stage[D, F], String] = {

    val extendedForms = stage.forms.collect {

      case cr: CalcResult[D, F] => missingDeclensions
         .map(declension => CalcResult(cr.data, CALC_DOWN_FROM_STEM, Set[CalcResult[D, F]](cr), Set(declension), this))

    }.flatten

    Left(Stage[D, F](extendedForms, this))
  }

  override def reverseCompute(stage: Stage[D, F]) = Left(stage)

  override def shortCode: String = "XT"
}