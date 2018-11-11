package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import com.hyenawarrior.OldNorseGrammar.grammar.nominal.SyncopeWordCalculator
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.{Composite, Stem, Suffix}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.{Default, Syncopated}
import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert._
import org.junit.rules.ErrorCollector
import org.junit.{Assert, Rule, Test}

/**
  * Created by HyenaWarrior on 2018.09.22..
  */
class TestMorpheme {

  val collector = new ErrorCollector

  @Rule
  def collectorDef = collector

  @Test
  def testNormalizing(): Unit = {

    assertEquals("grœnn", Word("groenn").asString)

    assertEquals("brautsk", Word("brauzk").asString)
  }

  @Test
  def splitSimpleMorpheme(): Unit = {

    val phs = Seq(
      SimpleVowel('a', Default),
      Consonant("b", Default),
      Consonant("c", Default),
      Consonant("d", Default)
    )

    val sm = SimpleMorpheme(phs, Composite)

    val opt = sm.splitAtToMorphemes(0)

    collector.checkThat("",     equalTo(opt.map(_._1.asString()).getOrElse("?")))
    collector.checkThat("abcd", equalTo(opt.map(_._2.asString()).getOrElse("?")))
  }

  @Test
  def splitSimpleMorpheme2(): Unit = {

    val phs = Seq(
      SimpleVowel('a', Default),
      Consonant("b", Default),
      Consonant("c", Default),
      Consonant("d", Default)
    )

    val sm = SimpleMorpheme(phs, Composite)

    val opt = sm.splitAtToMorphemes(4)

    collector.checkThat("abcd", equalTo(opt.map(_._1.asString()).getOrElse("?")))
    collector.checkThat("",     equalTo(opt.map(_._2.asString()).getOrElse("?")))
  }

  @Test
  def testDiphtongsMorpheme(): Unit = {

    val m = Morpheme("auðigr", MorphemeProperty.StemAndSuffix)

    val SimpleMorpheme(phs, _) = m

    assertEquals(Seq("au", "ð", "i", "g", "r"), phs.map(x => x.asString))
  }
}
