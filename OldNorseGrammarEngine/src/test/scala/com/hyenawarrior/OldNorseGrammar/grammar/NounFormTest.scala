package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{NounForm, NounStem}
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.02.03..
  */
class NounFormTest {

  @Test
  def testStrongMascJaStem(): Unit = {

    val fSN = NounForm.fromStringRepr("niðr", StrongStemClassMascA, SINGULAR -> NOMINATIVE)

    assertEquals(NounStem("niðj", StrongStemClassMascA), fSN.stem)
  }

  @Test
  def testStrongMascWaStem(): Unit = {

    val fSN = NounForm.fromStringRepr("sǫngr", StrongStemClassMascA, SINGULAR -> NOMINATIVE)

    assertEquals(NounStem("sangv", StrongStemClassMascA), fSN.stem)
  }

  @Test
  def testStrongFemWaStem(): Unit = {

    val stem = NounForm.fromStringRepr("ǫr", StrongStemClassFeminineA2, SINGULAR -> NOMINATIVE).stem

    assertEquals(NounStem("ar", StrongStemClassFeminineA2), stem)
  }

  @Test
  def testStrongMascRStemReverse(): Unit = {

    val stem = NounStem("mann", StrongStemClassMascR)

    assertEquals("maðr", NounForm.fromStem(stem, SINGULAR -> NOMINATIVE).strRepr)
    assertEquals("menn", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE).strRepr)
  }

  @Test
  def testStrongMascUStemReverse(): Unit = {

    val stem = NounStem("vall", StrongStemClassMascU)

    assertEquals("vǫllr",  NounForm.fromStem(stem, SINGULAR -> NOMINATIVE).strRepr)
    assertEquals("vellir", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE).strRepr)
  }

  @Test
  def testWeakMascRStem(): Unit = {

    val stem = NounForm.fromStringRepr("bóndi", WeakStemClassMascR, SINGULAR -> NOMINATIVE).stem

    assertEquals("bónd", stem.rootStr)
  }

  @Test
  def testWeakMascRStemReverse(): Unit = {

    val stem = NounStem("bónd", WeakStemClassMascR)

    val form = NounForm.fromStem(stem, PLURAL -> NOMINATIVE)

    assertEquals("bœndr", form.strRepr)
  }
}
