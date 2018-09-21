package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2018.06.12..
  */
class AdjectivalStemTest {

  @Test
  def testPositiveIndefiniteNyr(): Unit = {

    val adj = Adjective.from(
      Map(AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "nýr"),
      Set(POSITIVE_INDEFINITE))

    Assert.assertEquals("nýj", adj.stem.stemStr)

    println("nýr [NOM SG MASC INDEF] -> nýj-")
  }

  @Test
  def testPositiveIndefiniteHvass(): Unit = {

    val adj = Adjective.from(
      Map(AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "hvass"),
      Set(POSITIVE_INDEFINITE))

    Assert.assertEquals("hvass", adj.stem.stemStr)

    println("hvass [NOM SG MASC INDEF] -> hvass-")
  }

  @Test
  def testPositiveDefiniteNyrr(): Unit = {

    val adj1 = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_DEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "nýi"
    ), Set(POSITIVE_DEFINITE))

    Assert.assertEquals("nýj", adj1.stem.stemStr)

    println("nýi [NOM SG MASC DEF] -> nýj-")
  }

  @Test
  def testPositiveDefiniteHvass(): Unit = {

    val adj2 = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_DEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "hvassi"
    ), Set(POSITIVE_DEFINITE))

    Assert.assertEquals("hvass", adj2.stem.stemStr)

    println("hvassi [NOM SG MASC DEF] -> hvass-")
  }

  @Test
  def testComparativeHvass(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(COMPARATIVE, SINGULAR, MASCULINE, NOMINATIVE) -> "hvassari"
    ), Set(COMPARATIVE))

    Assert.assertEquals("hvass", adjective.stem.stemStr)

    println("hvassari [NOM SG MASC COMP] -> hvass-")
  }

  @Test
  def testComparativeNy(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(COMPARATIVE, SINGULAR, MASCULINE, NOMINATIVE) -> "nýrri"
    ), Set(COMPARATIVE))

    Assert.assertEquals("nýj", adjective.stem.stemStr)

    println("nýrri [NOM SG MASC COMP] -> nýj-")
  }

  @Test
  def testSuperlativeIndefiniteNyr(): Unit = {

    val adj = Adjective.from(Map(
      AdjectiveFormType(SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "nýjastr"
    ), Set(SUPERLATIVE_INDEFINITE))

    Assert.assertEquals("nýj", adj.stem.stemStr)

    println("nýjastr [NOM SG MASC SUPL INDEF] -> nýj-")
  }

  @Test
  def testSuperlativeIndefiniteHvass(): Unit = {

    val adj2 = Adjective.from(Map(
      AdjectiveFormType(SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "hvassastr"
    ), Set(SUPERLATIVE_INDEFINITE))

    Assert.assertEquals("hvass", adj2.stem.stemStr)

    println("hvassastr [NOM SG MASC SUPL INDEF] -> hvass-")
  }

  @Test
  def testSuperlativeDefiniteNyr(): Unit = {

    val adj = Adjective.from(Map(
      AdjectiveFormType(SUPERLATIVE_DEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "nýjasti"
    ), Set(SUPERLATIVE_DEFINITE))

    Assert.assertEquals("nýj", adj.stem.stemStr)

    println("nýjasti [NOM SG MASC SUPL DEF] -> nýj-")
  }

  @Test
  def testSuperlativeDefiniteHvass(): Unit = {

    val adj = Adjective.from(Map(
      AdjectiveFormType(SUPERLATIVE_DEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "hvassasti"
    ), Set(SUPERLATIVE_DEFINITE))

    Assert.assertEquals("hvass", adj.stem.stemStr)

    println("hvassasti [NOM SG MASC SUPL DEF] -> hvass-")
  }
}
