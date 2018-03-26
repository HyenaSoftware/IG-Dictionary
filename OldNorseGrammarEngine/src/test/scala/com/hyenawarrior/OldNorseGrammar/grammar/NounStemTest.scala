package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.DATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{StrongStemClassFeminineA2, StrongStemClassMascA, StrongStemClassNeuter}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{NounForm, NounStem}
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.02.03..
  */
class NounStemTest {

  @Test
  def testStrongMascAStem(): Unit = {

    assertEquals("hest", NounStem.fromStrRepr("hesta", StrongStemClassMascA).rootStr)
  }

  @Test
  def testStrongMascJAStem(): Unit = {

    assertEquals("nið", NounStem.fromStrRepr("niða", StrongStemClassMascA).rootStr)
  }

  @Test
  def testStrongMascWaStem(): Unit = {

    assertEquals("sǫng", NounStem.fromStrRepr("sǫnga", StrongStemClassMascA).rootStr)
    assertEquals("sǫngv", NounStem.fromStrRepr("sǫngva", StrongStemClassMascA).rootStr)
  }

  @Test
  def testStrongFemWoStem(): Unit = {

    // stem "ǫr" doesn't ends in velar consonant and it's not short
    //  so to restore the 'v' is reasonless
    assertEquals("ǫr", NounStem.fromStrRepr("ǫra", StrongStemClassFeminineA2).rootStr)
  }

  @Test
  def testStrongNeuterStemSieversLaw1(): Unit = {

    val nf = NounForm("ríki", SINGULAR -> DATIVE, isDefinite = false)
    assertEquals(s"Failed to recover from ${nf.strRepr}", "ríkj", NounStem.from(nf, StrongStemClassNeuter).rootStr)
  }

  @Test
  def testStrongNeuterStemSieversLaw2(): Unit = {

    val nf = NounForm("ríkjum", PLURAL -> DATIVE, isDefinite = false)
    assertEquals(s"Failed to recover from ${nf.strRepr}", "ríkj", NounStem.from(nf, StrongStemClassNeuter).rootStr)
  }

  @Test
  def testStrongNeuterJaStemKnjam(): Unit = {

    val nf = NounForm("knjám", PLURAL -> DATIVE, isDefinite = false)

    assertEquals("kné", NounStem.from(nf, StrongStemClassNeuter).rootStr)
  }
}
