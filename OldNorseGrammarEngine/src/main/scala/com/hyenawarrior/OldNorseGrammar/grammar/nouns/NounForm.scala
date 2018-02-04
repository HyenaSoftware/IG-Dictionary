package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: (GNumber, Case), stem: NounStem)


object NounForm {

  def fromStringRepr(str: String, stemClass: NounStemClass, declension: NounType): NounForm = (str, declension) match {

    case stemClass(stemStr) =>
      val nounStem = NounStem.fromStrRepr(stemStr, stemClass)
      fromStringRepr(nounStem, declension)

    case _ =>
      val declSuffix = stemClass inflection declension
      throw new RuntimeException(s"The word $str doesn't ends with $declSuffix.")
  }

  def fromStringRepr(stem: NounStem, declension: NounType): NounForm = {

    val Some(str) = stem.stemClass(stem.rootStr, declension)

    val strWithoutSVs = SemivowelDeletion(str)

    NounForm(strWithoutSVs, declension, stem)
  }

  def nounFrom(str: String) = 0


}
