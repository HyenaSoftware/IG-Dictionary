package com.hyenawarrior

import com.hyenawarrior.AdjectivalTestAux._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.{AdjectiveFormType, fromTuple}
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import org.junit.Assert._
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.06.12..
  */
class AdjectiveTest {

  val smn = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE)
  val sma = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, ACCUSATIVE)
  val smg = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, GENITIVE)
  val sfn = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, FEMININE, NOMINATIVE)
  val snn = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, NEUTER, NOMINATIVE)
  val TO_GENERATE = Set(POSITIVE_INDEFINITE)


  @Test
  def testGamall(): Unit = diff("gamal", Array(
    "gamall", "gǫmul",    "gamalt",
    "gamlan", "gamla",    "gamalt",
    "gǫmlum", "gamalli",  "gǫmlu",
    "gamals", "gamallar", "gamals",

    "gamlir", "gamlar",  "gǫmul",
    "gamla",  "gamlar",  "gǫmul",
    "gǫmlum", "gǫmlum",  "gǫmlum",
    "gamalla","gamalla", "gamalla"
    ), Array(
    "gamli", "gamla",  "gamla",
    "gamla", "gǫmlu",  "gamla",
    "gamla", "gǫmlu",  "gamla",
    "gamla", "gǫmlu",  "gamla",

    "gǫmlu",  "gǫmlu",  "gǫmlu",
    "gǫmlu",  "gǫmlu",  "gǫmlu",
    "gǫmlum", "gǫmlum", "gǫmlum",
    "gǫmlu",  "gǫmlu",  "gǫmlu"
  ))

  @Test
  def testGamall2(): Unit = {

    //import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.fromTuple

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "gamall",
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, FEMININE, NOMINATIVE) -> "gǫmul"
      //(POSITIVE_INDEFINITE, SINGULAR, NEUTER,    DATIVE) -> "gǫmlu"
    ))

    val optAdjForm1 = adjective.forms.get((POSITIVE_INDEFINITE, PLURAL,   NEUTER, NOMINATIVE))
    val optAdjForm2 = adjective.forms.get((POSITIVE_INDEFINITE, SINGULAR, NEUTER, DATIVE))

    assertEquals("gǫmul", optAdjForm1.map(_.strRepr).getOrElse("<missing>"))
    assertEquals("gǫmlu", optAdjForm2.map(_.strRepr).getOrElse("<missing>"))
  }

  @Test
  def testGamall3(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "gamall"
    ))

    val optAdjForm1 = adjective.forms.get((POSITIVE_INDEFINITE, PLURAL,   NEUTER, NOMINATIVE))
    val optAdjForm2 = adjective.forms.get((POSITIVE_INDEFINITE, SINGULAR, NEUTER, DATIVE))

    assertEquals("gǫmul", optAdjForm1.map(_.strRepr).getOrElse("<missing>"))
    assertEquals("gǫmlu", optAdjForm2.map(_.strRepr).getOrElse("<missing>"))
  }

  @Test
  def testGamall4(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, PLURAL,   FEMININE, ACCUSATIVE) -> "gamlar"
    ), Set(POSITIVE_INDEFINITE))

    assertEquals("gamall", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE)))
  }

  @Test
  def testAnnarr(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "annarr"
    ))

    assertEquals("ǫðru",  extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, NEUTER, DATIVE)))
    assertEquals("annarrar", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, FEMININE,  GENITIVE)))
  }

  @Test
  def testAnnarr2(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, PLURAL, FEMININE, NOMINATIVE) -> "aðrar",
      smg -> "annars"
    ), Set(POSITIVE_INDEFINITE))

    assertEquals("annar", adjective.stem.stemStr)

    assertEquals("ǫnnur", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, FEMININE,  NOMINATIVE)))
  }

  @Test
  def testConsonantAssimilationOnGodr() = assertEquals("góðr",   extract(Adjective.from(Map(sma -> "góðan"), TO_GENERATE), smn))

  @Test
  def testConsonantAssimilationOnGodr2() = assertEquals("góðan",   extract(Adjective.from(Map(smn -> "góðr"), TO_GENERATE), sma))

  @Test
  def testConsonantAssimilationOnVitr() = assertEquals("vitran",  extract(Adjective.from(Map(smn -> "vitr"), TO_GENERATE), sma))

  @Test
  def testConsonantAssimilationOnVakr() = assertEquals("vakrt",  extract(Adjective.from(Map(smn -> "vakr"), TO_GENERATE), snn))

  @Test
  def testConsonantAssimilationOnVaerr() = assertEquals("vært",  extract(Adjective.from(Map(smn -> "vaerr"), TO_GENERATE), snn))

  @Test
  def testAvoidCrash(): Unit = {

    try {

      Adjective.from(Map(smn -> ""), TO_GENERATE)

      fail()

    } catch {

      case e: Exception => ()
    }

    try {

      Adjective.from(Map(smn -> ""), TO_GENERATE)

      fail()

    } catch {

      case e: Exception => ()
    }
  }

  @Test
  def testÞyrstr(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "þyrstr"
    ))

    assertEquals("þyrstan",  extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, MASCULINE, ACCUSATIVE)))
    assertEquals("þyrstrar", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, FEMININE,  GENITIVE)))
  }

  @Test
  def testDyrr(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "dýrr",
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, FEMININE,  ACCUSATIVE) -> "dýra"
    ))

    assertEquals("dýr", adjective.stem.stemStr)

    assertEquals("dýran",  extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, MASCULINE, ACCUSATIVE)))
    assertEquals("dýrrar", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR, FEMININE,  GENITIVE)))
  }

  @Test
  def testNyrr(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, FEMININE, DATIVE) -> "nýrri"
    ), Set(POSITIVE_INDEFINITE))

    assertEquals("nýrr",    extract(adjective, (POSITIVE_INDEFINITE, SINGULAR,  MASCULINE,  NOMINATIVE)))
    assertEquals("nýtt",    extract(adjective, (POSITIVE_INDEFINITE, SINGULAR,  NEUTER,     NOMINATIVE)))
    assertEquals("nýrrar",  extract(adjective, (POSITIVE_INDEFINITE, SINGULAR,  FEMININE,   GENITIVE)))
    assertEquals("nýjum",   extract(adjective, (POSITIVE_INDEFINITE, PLURAL,    FEMININE,   DATIVE)))
  }

  @Test
  def testAudigr(): Unit = {

    val adjective = Adjective.from(Map(
      AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) -> "auðigr"
    ), Set(POSITIVE_INDEFINITE))

    assertEquals("auðgan", extract(adjective, (POSITIVE_INDEFINITE, SINGULAR,  MASCULINE,  ACCUSATIVE)))
  }

  private def extract(adj: Adjective, decl: AdjectiveFormType): String = adj.forms.get(decl).map(_.strRepr).getOrElse("<missing>")
}
