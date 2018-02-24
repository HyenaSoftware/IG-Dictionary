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

    val nounStem = NounStem.from(NounForm("niðr", SINGULAR -> NOMINATIVE), StrongStemClassMascA)

    assertEquals(NounStem("niðj", StrongStemClassMascA), nounStem)
  }

  @Test
  def testStrongMascWaStem(): Unit = {

    val nounStem = NounStem.from(NounForm("sǫngr", SINGULAR -> NOMINATIVE), StrongStemClassMascA)

    assertEquals(NounStem("sangv", StrongStemClassMascA), nounStem)
  }

  @Test
  def testStrongFemWaStem(): Unit = {

    val nounStem = NounStem.from(NounForm("ǫr", SINGULAR -> NOMINATIVE), StrongStemClassFeminineA2)

    assertEquals(NounStem("ar", StrongStemClassFeminineA2), nounStem)
  }

  @Test
  def testStrongMascRStemReverse(): Unit = {

    val stem = NounStem("mann", StrongStemClassMascR)

    assertEquals("maðr", NounForm.fromStem(stem, SINGULAR -> NOMINATIVE).strRepr)
    assertEquals("menn", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE).strRepr)
  }

  @Test
  def testStrongMascUStem(): Unit = {

    val nounStem = NounStem.from(NounForm("vǫllum", PLURAL -> DATIVE), StrongStemClassMascU)

    assertEquals(NounStem("vall", StrongStemClassMascU), nounStem)
  }

  @Test
  def testStrongMascUStem2(): Unit = {

    val stem = NounStem.from(NounForm("skildir", PLURAL -> NOMINATIVE), StrongStemClassMascU)

    assertEquals(NounStem("skeld", StrongStemClassMascU), stem)
  }

  @Test
  def testStrongMascUStemReverse(): Unit = {

    val stem = NounStem("vall", StrongStemClassMascU)

    assertEquals("vǫllr",  NounForm.fromStem(stem, SINGULAR -> NOMINATIVE).strRepr)
    assertEquals("vellir", NounForm.fromStem(stem, PLURAL   -> NOMINATIVE).strRepr)
  }

  @Test
  def testWeakMascRStem(): Unit = {

    val stem = NounStem.from(NounForm("bóndi", SINGULAR -> NOMINATIVE), WeakStemClassMascR)

    assertEquals("bónd", stem.rootStr)
  }

  @Test
  def testWeakMascRStemReverse(): Unit = {

    val stem = NounStem("bónd", WeakStemClassMascR)

    val form = NounForm.fromStem(stem, PLURAL -> NOMINATIVE)

    assertEquals("bœndr", form.strRepr)
  }
}
