package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: (GNumber, Case), stem: NounStem)


object NounForm {

  def fromStringRepr(str: String, stemClass: NounStemClass, declension: NounType): NounForm = {

    val declSuffix = stemClass inflection declension

    if(!str.endsWith(declSuffix)) {

      throw new RuntimeException(s"The word $str doesn't ends with $declSuffix.")
    }

    val stemStr = str stripSuffix declSuffix

    val nounStem = NounStem.fromStrRepr(stemStr, stemClass)

    fromStringRepr(nounStem, declension)
  }

  def fromStringRepr(stem: NounStem, declension: NounType): NounForm = {

    val str = stem.rootStr + stem.stemClass.inflection(declension)

    val strWithoutSVs = SemivowelDeletion(str)

    NounForm(strWithoutSVs, declension, stem)
  }

  def nounFrom(str: String) = 0


}
