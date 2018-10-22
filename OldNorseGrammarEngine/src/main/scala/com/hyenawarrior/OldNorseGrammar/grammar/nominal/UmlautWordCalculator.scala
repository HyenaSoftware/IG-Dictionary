package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.Stem
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.{Default, U_Umlauted}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology._

/**
  * Created by HyenaWarrior on 2018.10.03..
  */
object UmlautWordCalculator extends Calculator[Word, AdjectiveFormType] {

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left {

    val optTransform = AdjectiveTraits.transformationsFor(declension)
    val inflection = AdjectiveTraits.inflectionFor(declension)

    val nonProductiveUumlaut = optTransform match {
      case Some(U_Umlaut) => true
      case _ => false
    }

    val inflectionTriggerUmlaut = U_Umlaut.triggers.map(inflection.contains(_)).fold(false)(_ || _)

    if(inflectionTriggerUmlaut || nonProductiveUumlaut) {

      val newWord = word.transformMorphemes {

        case (mh, _) if mh is Stem =>

          mh.transformPhonemes[Vowel2]({ case ph: Vowel2 => ph }, {

            case (0, SimpleVowel('a', _)) => SimpleVowel('ǫ', U_Umlauted)
            case (1, SimpleVowel('a', _)) => SimpleVowel('u', U_Umlauted)
          })
      }

      Seq(newWord)

    } else { Seq(word) }
  }

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = {
    // gǫmlum -> gamlum/gamlam

    val optStressedVowel = word.selectMorpheme(Stem)
      .map(_.phonemes.collect { case ph if ph.isVowel => ph.asInstanceOf[Vowel2] })

    optStressedVowel match {
      case Some(Seq()) => Right(s"Word '${word.asString}' does not have any vowel in the 'stem' morpheme.")
      case None => Right(s"Word '${word.asString}' does not have any morpheme")

      case Some(stressedVowels) =>

        val possibleUmlaut = possibleUmlautFor(declension, stressedVowels.head)

        val newWord = possibleUmlaut match {

          case Some(U_Umlaut) =>

          word.transformMorphemes {

            case (mh, _) if mh is Stem =>

            mh.transformPhonemes[Vowel2]({ case ph: Vowel2 => ph }, {

              case (0 | 1, v) => SimpleVowel('a', Default)
            })
          }

          case _ => word
        }

        Left(Seq(newWord))
    }
  }

  private def possibleUmlautFor(declension: AdjectiveFormType, stressedVowel: Vowel2): Option[Umlaut] = {

    val optTransform = AdjectiveTraits.transformationsFor(declension)

    (optTransform, stressedVowel) match {
      // non-productive transformation
      case (Some(U_Umlaut), _) => Some(U_Umlaut)
      // productive U-umlaut
      case (None, SimpleVowel('ǫ', _)) => Some(U_Umlaut)
      case _ => None
    }
  }

  override def shortCode: String = "UML"
}
