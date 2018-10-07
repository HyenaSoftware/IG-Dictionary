package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.Stem
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Default
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Morpheme, Semivowel, Word}

/**
  * Created by HyenaWarrior on 2018.10.07..
  */
object SemivowelWordDeletionCalculator extends Calculator[Word, AdjectiveFormType] {

  private val C = "[bdðfghjklmnprstvxzþ]"
  private val pattern = s"^$C*ý$$".r

  override def compute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left {

    val inflection = inflectionWithComparsionFor(declension)

    val newWord = word.transformMorphemes {

      case (mh, i) if mh is Stem =>
        val str = SemivowelDeletion(mh.asString() + inflection).stripSuffix(inflection)
        Morpheme(str, mh.morphemeProperty)
    }

    Seq(newWord)
  }

  override def reverseCompute(word: Word, declension: AdjectiveFormType, stage: Stage[Word, AdjectiveFormType]) = Left {

    val newWord = word.transformMorphemes {

      case (mh, i) if mh is Stem =>

        mh.asString() match {

          case pattern() => mh + Semivowel('j', Default)
          case _ => mh
        }
    }

    Seq(newWord)
  }

  override def shortCode = "SVD"
}
