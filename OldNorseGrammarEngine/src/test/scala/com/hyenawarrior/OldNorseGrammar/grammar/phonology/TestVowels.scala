package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2017.10.23..
  */
class TestVowels {

  @Test
  def testVowels(): Unit = {

    assertTrue(Vowel.isBackVowel('a'))
  }

  @Test
  def testVowelProperties(): Unit = {

    val v = Vowel(Vowel.FRONT | Vowel.LABIAL, Vowel.SHORT)

    assertFalse(v.isBack)
    assertTrue(v.isFront)

    assertFalse(v.isNonLabial)
    assertTrue(v.isLabial)
  }
}
