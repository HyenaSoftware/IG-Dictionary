package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{ConsonantAssimilation, SemivowelDeletion}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: NounType)

object NounForm {

  def fromStem(stem: NounStem, declension: NounType): NounForm = {

    val NounStem(rootStr, stemClass) = stem

    //
    val canBeUmlauted = theseCanCauseUUmlaut(rootStr + stemClass.inflection(declension))

    // apply productive U-umlaut
    val umlautedRootStr = if(canBeUmlauted) U_Umlaut(rootStr).getOrElse(rootStr) else rootStr

    // inflect
    val Some(inflectedForm) = stemClass(umlautedRootStr, declension)

    // SVD
    val strWithSVs = SemivowelDeletion(inflectedForm)

    // consonant assimilation
    val strConsAssimilation =
      // perhaps the presence of the thematic vowel in the Proto-Germanic blocked the assimilation of the -r in the
      // Old Norse as well: *walþuz -> *vǫllur -> vǫllr (instead of vǫll)
      if(stemClass.thematicVowel.isDefined && strWithSVs.endsWith("r"))
        strWithSVs else ConsonantAssimilation(strWithSVs)

    NounForm(strConsAssimilation, declension)
  }

  def nounFrom(str: String) = 0
}
