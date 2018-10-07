package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.ConsonantAssimilation2
import com.hyenawarrior.OldNorseGrammar.grammar.phonology
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Word

/**
  * Created by HyenaWarrior on 2018.10.03..
  */
object ConsonantWordAssimilationCalculator extends Calculator[phonology.Word, AdjectiveFormType] {

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]): Either[Seq[Word], String] = Left {

    val rs = ConsonantAssimilation2.transform(word)

    if(rs.nonEmpty) rs else Seq(word)
  }

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]): Either[Seq[Word], String] = Left {

    val rs = ConsonantAssimilation2.reverse(word)

    if(rs.nonEmpty) rs else Seq(word)
  }

  override def shortCode: String = "CA"
}
