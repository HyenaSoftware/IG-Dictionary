package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{NounForm, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{StrongStemClassFeminineA1, StrongStemClassFeminineA2, StrongStemClassMascA}
import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

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
}
