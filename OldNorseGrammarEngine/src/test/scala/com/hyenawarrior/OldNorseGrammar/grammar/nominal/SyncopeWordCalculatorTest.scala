package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.{Stem, Suffix}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Syncopated
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Diphtong, Morpheme, SimpleVowel, Word}
import org.junit.Assert._
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2018.10.07..
  */
class SyncopeWordCalculatorTest {

  @Test
  def testSyncopeTest(): Unit = {

    val w1 = Word("gamal", Stem) + Word("an", Suffix)

    val w2 = w1.transformMorphemes {

      case (mh, _) if mh.morphemeProperty == Stem =>

      mh.transformPhomemes(_.getClass, {
        case (1, SimpleVowel(v, _)) => SimpleVowel(v, Syncopated)
        case (1, Diphtong(v, _)) => Diphtong(v, Syncopated)
      })
    }

    assertEquals("gamlan", w2.asString)
  }

  @Test
  def testSyncopeTest2(): Unit = {

    val word = new Word(Seq(
      Morpheme("gamal", Stem),
      Morpheme("r", Suffix)
    ))

    val stage = Stage(Seq(), SyncopeWordCalculator)

    val calculator = SyncopeWordCalculator.asInstanceOf[Calculator[Word, AdjectiveFormType]]

    calculator.compute(word, (POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE), stage) match {

      case Left (Seq(word2)) =>
      assertEquals ("gamalr", word2.asString)

      case Right(error) =>
      Assert.fail(s"Unexpected error: $error")
    }
  }
}
