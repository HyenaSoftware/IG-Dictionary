package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounStem
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{StrongStemClassFeminineA1, StrongStemClassFeminineA2, StrongStemClassMascA}
import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

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
}
