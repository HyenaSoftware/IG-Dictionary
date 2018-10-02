package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits.inflectionWithComparsionFor
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.Syncope
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.Syncope.adjustSyncopatedInflection
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut, Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._
import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Word, morphophonology}

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object SyncopeCalculator extends Calculator[String, AdjectiveFormType] {

  override def compute(str: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]) = Left { Seq {

    val inflection = inflectionWithComparsionFor(declension)

    Syncope.transform(str, inflection)
  }}

  // TODO: move these to a common place
  private val C = "[bdðfghjklmnprstvxzþ]"
  private val patternForEndsWith2Cons = s"$C{2}$$".r

  private def possibleSyncopatedVowel(declension: AdjectiveFormType, sys: List[Syllable]): String = {

    val possibleSecondVowel = possibleUmlautsOf(declension, sys) match {

      case Some(U_Umlaut) => "u"
      case Some(I_Umlaut) => "i"
      case _ => "a"
    }

    possibleSecondVowel
  }

  private def endsWithTwoConsonant(str: String): Boolean = patternForEndsWith2Cons.findFirstIn(str).nonEmpty

  override def reverseCompute(form: String, declension: AdjectiveFormType, stage: Stage[String, AdjectiveFormType]) = {

    //val isDiSyllabic = stage.countOfSyllables == 2
    val word = Word(form)
    val Word(sys) = word

    /*
      nýrra > nýr+ra

      gjǫflum > gjǫful+um > gjǫfulum
      gjǫflari > gjǫful+ari > gjǫfulari
      lengari > lengri

      auðgir > auðigir
      auðigri > auðigari
      lengri > lengari

      auðg-[ir]       > auð-ig-[ir]
      ! auðg-[ar]-[i]   > auð-ig-[ar]-[i]
      ~ auð-ig[r]-[i]   > auð-ig-[ar]-[i]
      gjǫfl-[um]      > gjǫf-ul-[um]
      gjǫfl-[ar]-[i]  > gjǫf-ul-[ar]-[i]
      lengr-[i]       > leng-[ar]-[i]

      langr - leng(a)ri - leng(a)st
      ! auðigr - auð(i)gari - auð(i)gastr
      ~ auðigr - auðig(a)ri - auðig(a)str
    */

    //val ts = word.traditionalSyllables()

    val possibleSecondVowel = possibleSyncopatedVowel(declension, sys)

    val inflection = inflectionWithComparsionFor(declension)
    val startsWithVowel = inflection.headOption.exists(isVowel)

    val adjustedInflection = adjustSyncopatedInflection(inflection)
    val formWithoutInflection = morphophonology.stripSuffix(form, adjustedInflection)

    val stemEndsWithTwoConsonants = endsWithTwoConsonant(formWithoutInflection)

    val form2 = if(startsWithVowel && stemEndsWithTwoConsonants) {

      // -CC|V- : possibly the stem is syncopated
      val idx = formWithoutInflection.length - 1
      val (s1, s2) = form splitAt idx

      val lastStemChar = s1.lastOption.getOrElse('$')

      if(isVowel(lastStemChar)) {

        form

      } else { s1 + possibleSecondVowel + s2 }

    } else { form }

    Left(Seq(form2))
  }

  private def possibleUmlautsOf(declension: AdjectiveFormType, sys: List[Syllable]): Option[Umlaut] = {

    val optTransform = AdjectiveTraits.transformationsFor(declension)

    val possibleUmlaut: Option[Umlaut] = optTransform match {
      // non-productive transformation
      case Some(U_Umlaut) => Some(U_Umlaut)
      // productive U-umlaut
      case None if sys.head.nucleus == "ǫ" => Some(U_Umlaut)
      case _ => None
    }

    possibleUmlaut
  }

  override def shortCode: String = "SYNC"
}