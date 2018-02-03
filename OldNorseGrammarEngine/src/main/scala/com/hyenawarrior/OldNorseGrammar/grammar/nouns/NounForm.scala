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

    val stemStr = str stripSuffix (stemClass inflection declension)

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
