package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object SemivowelDeletionCalculator extends Calculator[AdjectiveFormType] {

  private val C = "[bdðfghjklmnprstvxzþ]"
  private val pattern = s"^($C*ý)([^j].*)?$$".r

  /*
    FIXME: reverse-SVD should goes after the inflectioning: otherwise it is unknown the stem ends with a long vowel or not
    FIXME: compute-SVD should goes after the inflectioning: othersie it is unknown the stem inflectional suffix can keep back the semivowel
   */

  override def compute(str: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]): Either[Seq[String], String] = {

    //val jidx = str.indexWhere(isVowel)

    val rs = SemivowelDeletion(str)

    Left(Seq(rs))
  }

  override def reverseCompute(str: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]): Either[Seq[String], String] = {

    //val iv = str.indexWhere(isVowel)
    //val firstNucleus = str.substring(iv).takeWhile(isVowel)

    //val originalStem = str + (if(str endsWith "ý") { "j" } else { "" })

    // still imperfect: does the stem ends a stressed vowel?
    val originalStem = str match {

      case pattern(p1, p2) => p1 + "j" + p2
      case _ => str
    }

    /*
    val isFrontMutated = I_Umlaut.targetVowels contains firstNucleus
    val isBackMutated = U_Umlaut.targetVowels contains firstNucleus

    val semivowel = if(isFrontMutated) { "j" } else if(isBackMutated) { "v"} else { "" }

    val alreadyHasIt = str endsWith semivowel
    val originalStem = str + (if(alreadyHasIt) "" else semivowel)
    */

    Left(Seq(originalStem))
  }

  override def shortCode: String = "SVD"
}
