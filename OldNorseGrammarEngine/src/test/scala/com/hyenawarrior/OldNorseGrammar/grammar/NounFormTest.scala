package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{NounForm, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.StrongStemClassMascA
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

    assertEquals(NounStem("sǫngv", StrongStemClassMascA), fSN.stem)
  }
}
