package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{FixJAugmentation, FixVAugmentation}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: (GNumber, Case), stem: NounStem)


object NounForm {

  def fromStringRepr(str: String, stemClass: NounStemClass, declension: NounType): NounForm = try {

    val stemClass(uninflectedStr) = (str, declension)

    val isLocallyTriggeredUmlaut = theseCanCauseUUmlaut(stemClass.inflection(declension))
    val isNonProductiveUmlaut = stemClass.transformationFor(declension).contains(U_Umlaut)

    val (stemStr, optUmlaut) = uninflectedStr match {

      case U_Umlaut(s) if uninflectedStr != s =>

        val globalUmlaut = if (isNonProductiveUmlaut || isLocallyTriggeredUmlaut) None else Some(U_Umlaut)
        s -> globalUmlaut

      case s => s -> None
    }

    // if optUmlaut!=None then a semivowel caused a productive umlaut mutation in every form of the noun

    //
    val rootStr = NounStem.removeThematicVowel(stemStr, stemClass)
    val augmentedRootStr = augment(rootStr, optUmlaut)
    val nounStem = NounStem(augmentedRootStr, stemClass)

    fromStem(nounStem, declension)

  } catch {

    case e: Exception =>
      val declSuffix = stemClass inflection declension
      throw new RuntimeException(s"The word $str doesn't ends with $declSuffix.")
  }

  // basically it's the reverse SVD
  private def augment(stemStr: String, optUmlaut: Option[Umlaut]): String = (stemStr, optUmlaut) match {

    case (FixVAugmentation(fixedStemStr), _) => fixedStemStr
    case (FixJAugmentation(fixedStemStr), _) => fixedStemStr
    case (_, Some(U_Umlaut)) => stemStr + "v"
    case _ => stemStr
  }

  def fromStem(stem: NounStem, declension: NounType): NounForm = {

    val NounStem(rootStr, stemClass) = stem

    //
    val canBeUmlauted = theseCanCauseUUmlaut(rootStr + stemClass.inflection(declension))
    val umlautedRootStr = if(canBeUmlauted) U_Umlaut(rootStr).getOrElse(rootStr) else rootStr

    val Some(inflectedForm) = stemClass(umlautedRootStr, declension)

    val strWithoutSVs = SemivowelDeletion(inflectedForm)

    NounForm(strWithoutSVs, declension, stem)
  }

  private def theseCanCauseUUmlaut(str: String): Boolean = U_Umlaut.triggers.exists { str.contains(_) }

  def nounFrom(str: String) = 0
}
