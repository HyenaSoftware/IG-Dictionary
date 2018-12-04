package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.{Stem, StemSuffix, Suffix}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Syncopated
import com.hyenawarrior.OldNorseGrammar.grammar.phonology._

/**
  * Created by HyenaWarrior on 2018.10.02..
  */
object SyncopeWordCalculator extends Calculator[Word, AdjectiveFormType]  {

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = {

    val hasVowelAfterTheStem = word.morphemes
      .find(m => !(m is Stem))
      .exists(mh => mh.phonemes.headOption.exists(_.isVowel))

    if(hasVowelAfterTheStem) {

      val newWord = word.transformMorphemes {
        case (mh @ SimpleMorpheme(_, Stem | StemSuffix), _) =>
          mh.transformPhonemes[Vowel2](
            { case ph: Vowel2 => ph },
            { case (1, v) => v copyWithPropertyOf Syncopated }
          )
      }

      Left(Seq(newWord))

    } else {

      Left(Seq(word))
    }
  }

  override def shortCode: String = "SYNC"

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = {

    val optStemMorpheme = word.selectMorpheme(Stem)
    val optSuffixMorpheme = word.selectMorpheme(Suffix)

    if(optStemMorpheme.isEmpty) {

      Right("Error: stem is missing")

    } else if(optSuffixMorpheme.isEmpty) {

      Right("Error: suffix is missing")

    } else Left {

      val Some(stemMorpheme) = optStemMorpheme
      val Some(suffixMorpheme) = optSuffixMorpheme

      val lastCons = stemMorpheme.phonemes.takeRight(2).collect { case c: Consonant => c }
      val stemEndsWithTwoConsonants = lastCons.size == 2
      val stemEndsWithDoubledConsonants = lastCons.toSet.size == 1
      val suffixStartsWithVowel = suffixMorpheme.phonemes.headOption.exists(_.isVowel)

      if(suffixStartsWithVowel && stemEndsWithTwoConsonants) {

        val newWord = word.transformMorphemes {

          case (mh, _) if mh is Stem =>
            val stressedVowel = mh.phonemes
              .collect { case ph if ph.isVowel => ph.asInstanceOf[Vowel2] }
              .head

            val possibleSecondVowel = possibleUnstressedVowel(declension, stressedVowel)

            val (phs1, phs2) = mh.splitAtTo(-1).get
            val seq = (phs1 :+ possibleSecondVowel) ++ phs2
            SimpleMorpheme(seq, Stem)

          case (mh, _) if mh is Suffix => mh.transformPhonemesRight[Vowel2](

              { case ph: Vowel2 => ph },
              { case (1, v) => v copyWithPropertyOf Syncopated }
            )
        }

        if(stemEndsWithDoubledConsonants) {

          Seq(word, newWord)

        } else {

          Seq(word, newWord)
        }

      } else {

        Seq(word)
      }
    }
  }

  private def possibleUnstressedVowel(declension: AdjectiveFormType, stressedVowel: Vowel2): SimpleVowel = {

    val optTransform = AdjectiveTraits.transformationsFor(declension)

    val chr = (optTransform, stressedVowel) match {
      // non-productive transformation
      case (Some(U_Umlaut), _) => 'u'
      // productive U-umlaut
      case (None, SimpleVowel('ǫ', _)) => 'u'
      //case ? => "i"
      case _ => 'a'
    }

    SimpleVowel(chr, PhonemeProperty.Syncopated)
  }

}
