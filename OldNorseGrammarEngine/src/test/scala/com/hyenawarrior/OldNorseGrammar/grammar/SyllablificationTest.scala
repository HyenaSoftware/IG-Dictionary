package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length
import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2018.02.05..
  */
class SyllablificationTest {

  private def check(word: String, expectedSyllables: Syllable*): Unit = {

    val Syllables(syllables) = word

    if(expectedSyllables.size != syllables.size) {

      Assert.fail(s"Incorrect number of syllables, it has ${syllables.size}, expected is ${expectedSyllables.size}.")
    }

    for((esy, gsy) <- expectedSyllables zip syllables) {

      assertEquals(esy, gsy)
    }
  }

  @Test
  def testSyllableLength(): Unit = check("ǫrvar",
    Syllable("", "ǫ", "r", isStressed = true, Length.LONG),
    Syllable("v", "a", "r", isStressed = false, Length.LONG))

  @Test
  def testSyllablificationShort(): Unit = check("vera",
    Syllable("v", "e", "", isStressed = true, Length.SHORT),
    Syllable("r", "a", "", isStressed = false, Length.SHORT))

  @Test
  def testSyllablificationShort2(): Unit = check("bit",
    Syllable("b", "i", "t", isStressed = true, Length.LONG))

  @Test
  def testSyllablificationShort3(): Unit = check("kallaði",
      Syllable("k", "a", "l", isStressed = true, Length.LONG),
      Syllable("l", "a", "", isStressed = false, Length.SHORT),
      Syllable("ð", "i", "", isStressed = false, Length.SHORT))

  @Test
  def testSyllablificationLong(): Unit = check("bú",
    Syllable("b", "ú", "", isStressed = true, Length.LONG))

  @Test
  def testSyllablificationLong2(): Unit = check("bíta",
    Syllable("b", "í", "", isStressed = true, Length.LONG),
    Syllable("t", "a", "", isStressed = false, Length.SHORT))

  // extrametrically long
  @Test
  def testSyllablificationLong3(): Unit = check("sól",
    Syllable("s", "ó", "l", isStressed = true, Length.OVERLONG))

  @Test
  def testSyllablificationLongDiphtong(): Unit = check("ey",
    Syllable("", "ey", "", isStressed = true, Length.LONG))

  @Test
  def testSyllablificationLongCons(): Unit = check("kasta",

    Syllable("k", "a", "s", isStressed = true, Length.LONG),
    Syllable("t", "a", "", isStressed = false, Length.SHORT))

  @Test
  def testSyllablificationLongCons2(): Unit = check("hest",

    Syllable("h", "e", "st", isStressed = true, Length.LONG))

  @Test
  def testSyllablificationLongCons3(): Unit = check("gamali",

    Syllable("g", "a", "", isStressed = true, Length.SHORT),
    Syllable("m", "a", "", isStressed = false, Length.SHORT),
    Syllable("l", "i", "", isStressed = false, Length.SHORT))

  @Test
  def testSyllablificationLongCons4(): Unit = check("hamarr",

    Syllable("h", "a", "", isStressed = true, Length.SHORT),
    Syllable("m", "a", "rr", isStressed = false, Length.LONG))

  @Test
  def testSyllablificationOverLong(): Unit = check("nátt",
    Syllable("n", "á", "tt", isStressed = true, Length.OVERLONG))

  @Test
  def testSyllablificationOverLong2(): Unit = check("haust",
    Syllable("h", "au", "st", isStressed = true, Length.OVERLONG))
}
