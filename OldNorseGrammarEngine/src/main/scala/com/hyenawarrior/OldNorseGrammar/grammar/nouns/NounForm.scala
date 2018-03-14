package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.PoSForm
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.FEMININE
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{ConsonantAssimilation, SemivowelDeletion, Syncope, VowelDeletion}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.auxiliary.TupleEx._

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: NounType, isDefinite: Boolean) extends PoSForm

object NounForm {

  def fromStem(stem: NounStem, declension: NounType, isDefinite: Boolean): NounForm = {

    val NounStem(rootStr, stemClass) = stem

    //
    val canBeUmlauted = theseCanCauseUUmlaut(rootStr + stemClass.inflection(declension))

    // apply productive U-umlaut
    val umlautedRootStr = if(canBeUmlauted) U_Umlaut(rootStr).getOrElse(rootStr) else rootStr

    // inflect
    val Some(inflectedForm) = stemClass(umlautedRootStr, declension, isDefinite)

    val syncopedStr = stem.stemClass.associatedGender +: declension match {

      case (FEMININE, SINGULAR, _) => inflectedForm
      case _ => Syncope(inflectedForm)
    }

    // SVD
    val strWithSVs = SemivowelDeletion(syncopedStr)

    //
    val strAfterVowelDeletion = VowelDeletion(strWithSVs)

    // consonant assimilation
    val strConsAssimilation =
      // perhaps the presence of the thematic vowel in the Proto-Germanic blocked the assimilation of the -r in the
      // Old Norse as well: *walþuz -> *vǫllur -> vǫllr (instead of vǫll)
      if(stemClass.thematicVowel.isDefined && (strAfterVowelDeletion endsWith "r"))
        strAfterVowelDeletion else ConsonantAssimilation(strAfterVowelDeletion)

    NounForm(strConsAssimilation, declension, isDefinite)
  }

  def nounFrom(str: String) = 0
}
