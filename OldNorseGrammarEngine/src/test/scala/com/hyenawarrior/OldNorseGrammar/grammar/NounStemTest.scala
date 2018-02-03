package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounStem
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.StrongStemClassMascA
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

    assertEquals("nirðj", NounStem.fromStrRepr("nirð", StrongStemClassMascA).rootStr)
  }

  @Test
  def testStrongMascWaStem(): Unit = {

    assertEquals("sǫngv", NounStem.fromStrRepr("sǫnga", StrongStemClassMascA).rootStr)
    assertEquals("sǫngv", NounStem.fromStrRepr("sǫng", StrongStemClassMascA).rootStr)
  }
}
