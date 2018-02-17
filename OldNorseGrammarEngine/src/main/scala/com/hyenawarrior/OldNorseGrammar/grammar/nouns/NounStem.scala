package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.ConsonantAssimilation
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{FixJAugmentation, FixVAugmentation}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
  * Created by HyenaWarrior on 2018.01.31..
  */
case class NounStem(rootStr: String, stemClass: NounStemClass)

object NounStem {

  def from(nounForm: NounForm, stemClass: NounStemClass): NounStem = try {

    // undo consonant assimilation
    val consAssimilatedStrs = ConsonantAssimilation invert nounForm.strRepr

    // remove inflection
    val uninflectedStrs = consAssimilatedStrs
      .map(a => a -> nounForm.declension)
      .flatMap {

        case stemClass(ustr) => Some(ustr)
        case _ => None
      }

    // reverse-SVD should be done here?

    val uninflectedStr = uninflectedStrs.head

    val isLocallyTriggeredUmlaut = nouns.theseCanCauseUUmlaut(stemClass inflection nounForm.declension)
    val isNonProductiveUmlaut = stemClass.transformationFor(nounForm.declension).contains(U_Umlaut)

    // reverse U-umlaut
    val (stemUnUmlautedStr, optUmlaut) = uninflectedStr match {

      case U_Umlaut(s) if uninflectedStr != s =>

        val globalUmlaut = if (isNonProductiveUmlaut || isLocallyTriggeredUmlaut) None else Some(U_Umlaut)
        s -> globalUmlaut

      case s => s -> None
    }

    // if optUmlaut!=None then a semivowel caused a productive umlaut mutation in every form of the noun

    //
    val rootStr = removeThematicVowel(stemUnUmlautedStr, stemClass)

    // undo SVD
    val augmentedRootStr = augment(rootStr, optUmlaut)

    // create the noun stem
    NounStem(augmentedRootStr, stemClass)

  } catch {

    case e: Exception =>
      println(e)
      val declSuffix = stemClass inflection nounForm.declension
      val strRepr = nounForm.strRepr
      throw new RuntimeException(s"The word $strRepr doesn't ends with $declSuffix.", e)
  }

  // basically it's the reverse SVD
  private def augment(stemStr: String, optUmlaut: Option[Umlaut]): String = (stemStr, optUmlaut) match {

    case (FixVAugmentation(fixedStemStr), _) => fixedStemStr
    case (FixJAugmentation(fixedStemStr), _) => fixedStemStr
    case (_, Some(U_Umlaut)) => stemStr + "v"
    case _ => stemStr
  }

  def fromStrRepr(stemStr: String, stemClass: NounStemClass): NounStem = {

    val rootStr = removeThematicVowel(stemStr, stemClass)

    NounStem(rootStr, stemClass)
  }

  def removeThematicVowel(stemStr: String, stemClass: NounStemClass): String = {

    val thematicVowel = stemClass.thematicVowel.getOrElse("")

    stemStr stripSuffix thematicVowel
  }
}