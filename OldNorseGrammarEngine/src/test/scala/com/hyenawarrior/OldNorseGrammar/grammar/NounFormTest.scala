package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{DATIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{PLURAL, SINGULAR}
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

    val nounStem = NounStem.from(NounForm("niðr", SINGULAR -> NOMINATIVE, false), StrongStemClassMascA)

    assertEquals(NounStem("niðj", StrongStemClassMascA), nounStem)
  }

  @Test
  def testStrongMascWaStem(): Unit = {

    val nounStem = NounStem.from(NounForm("sǫngr", SINGULAR -> NOMINATIVE, false), StrongStemClassMascA)

    assertEquals(NounStem("sangv", StrongStemClassMascA), nounStem)
  }

  @Test
  def testStrongFemWaStem(): Unit = {

    val nounStem = NounStem.from(NounForm("ǫr", SINGULAR -> NOMINATIVE, false), StrongStemClassFeminineA2)

    assertEquals(NounStem("ar", StrongStemClassFeminineA2), nounStem)
  }

  @Test
  def testStrongMascRStemReverse(): Unit = {

    val stem = NounStem("mann", StrongStemClassMascR)

    assertEquals("maðr", NounForm.fromStem(stem, SINGULAR -> NOMINATIVE, false).strRepr)
    assertEquals("menn", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE, false).strRepr)
  }

  @Test
  def testStrongMascUStem(): Unit = {

    val nounStem = NounStem.from(NounForm("vǫllum", PLURAL -> DATIVE, false), StrongStemClassMascU)

    assertEquals(NounStem("vall", StrongStemClassMascU), nounStem)
  }

  @Test
  def testStrongMascUStem2(): Unit = {

    val stem = NounStem.from(NounForm("skildir", PLURAL -> NOMINATIVE, false), StrongStemClassMascU)

    assertEquals(NounStem("skeld", StrongStemClassMascU), stem)
  }

  @Test
  def testStrongMascUStemReverse(): Unit = {

    val stem = NounStem("vall", StrongStemClassMascU)

    assertEquals("vǫllr",  NounForm.fromStem(stem, SINGULAR -> NOMINATIVE, false).strRepr)
    assertEquals("vellir", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE, false).strRepr)
  }

  @Test
  def testWeakMascRStem(): Unit = {

    val stem = NounStem.from(NounForm("bóndi", SINGULAR -> NOMINATIVE, false), WeakStemClassMascR)

    assertEquals("bónd", stem.rootStr)
  }

  @Test
  def testWeakMascRStemReverse(): Unit = {

    val stem = NounStem("bónd", WeakStemClassMascR)

    val form = NounForm.fromStem(stem, PLURAL -> NOMINATIVE, false)

    assertEquals("bœndr", form.strRepr)
  }
}
