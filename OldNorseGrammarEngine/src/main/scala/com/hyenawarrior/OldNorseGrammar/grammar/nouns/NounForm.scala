package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{ConsonantAssimilation, SemivowelDeletion}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{FixJAugmentation, FixVAugmentation}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounStem.removeThematicVowel
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
case class NounForm(strRepr: String, declension: (GNumber, Case), stem: NounStem)


object NounForm {

  def fromStringRepr(str: String, stemClass: NounStemClass, declension: NounType): NounForm = try {

    // undo consonant assimilation
    val consAssimilatedStrs = ConsonantAssimilation invert str

    // remove inflection
    val uninflectedStrs = consAssimilatedStrs
      .map(a => a -> declension)
      .flatMap {

        case stemClass(ustr) => Some(ustr)
        case _ => None
      }

    // reverse-SVD should be done here?

    val uninflectedStr = uninflectedStrs.head

    val isLocallyTriggeredUmlaut = theseCanCauseUUmlaut(stemClass.inflection(declension))
    val isNonProductiveUmlaut = stemClass.transformationFor(declension).contains(U_Umlaut)

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
    val stem = NounStem(augmentedRootStr, stemClass)

    // create the "official" representation of this noun-form from the constructed stem
    fromStem(stem, declension)

  } catch {

    case e: Exception =>
      println(e)
      val declSuffix = stemClass inflection declension
      throw new RuntimeException(s"The word $str doesn't ends with $declSuffix.", e)
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

    NounForm(strConsAssimilation, declension, stem)
  }

  private def theseCanCauseUUmlaut(str: String): Boolean = {

    val i = str.indexWhere(isVowel) match {

      case -1 => 0
      case i => i
    }

    val str2 = str substring i

    U_Umlaut.triggers.exists { str2.contains(_) }
  }

  def nounFrom(str: String) = 0
}
