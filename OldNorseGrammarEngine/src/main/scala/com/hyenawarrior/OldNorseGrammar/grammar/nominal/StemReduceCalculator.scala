package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{CALC_DOWN_FROM_STEM, CalcError, CalcResult, Stage}
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.StageCalculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{SimpleMorpheme, Word}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Default

/**
  * Created by HyenaWarrior on 2018.10.23..
  */
object StemReduceCalculator extends StageCalculator[Word, AdjectiveFormType] {

  override def compute(stage: Stage[Word, AdjectiveFormType]): Either[Stage[Word, AdjectiveFormType], String] = {

    val newForms = stage.forms.flatMap {

      case cr: CalcResult[Word, AdjectiveFormType] if cr.calcDirection == CALC_DOWN_FROM_STEM
        => Some(CalcResult(cr.data, cr.calcDirection, Set(cr), cr.declensions, StemReduceCalculator))
      case ce: CalcError => Some(ce)
      case _ => None
    }

    Left(Stage(newForms, StemReduceCalculator))
  }

  override def reverseCompute(stage: Stage[Word, AdjectiveFormType]): Either[Stage[Word, AdjectiveFormType], String] = {

    val groupedForms = stage.forms.groupBy {

      case cr: CalcResult[Word, AdjectiveFormType] => CalcResult[Word, AdjectiveFormType](resetPhonemes(cr.data), cr.calcDirection, Set(), Set(), null)
    }

    val forms = groupedForms.values.flatMap { ciSeq =>
      val crs = ciSeq.collect {

        case cr: CalcResult[Word, AdjectiveFormType] => cr
      }

      val declensions = crs.flatMap(_.declensions)

      Some(CalcResult(resetPhonemes(crs.head.data), crs.head.calcDirection, crs.toSet, declensions.toSet, StemReduceCalculator))
    }

    Left(Stage(forms.toSeq, StemReduceCalculator))
  }

  private def resetPhonemes(word: Word): Word = new Word(word.morphemes.map {

    case SimpleMorpheme(phonemes, prop) => SimpleMorpheme(phonemes.map(_ copyWithPropertyOf Default), prop)
  })

  override def shortCode: String = "RDC"
}
