package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.Case.{ACCUSATIVE, DATIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{NounStemClass, StrongStemClassMascA, StrongStemClassMascI}
import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class NounTest {

  @Test
  def testStrongMascAStemInflectionForWolf(): Unit = {

    val root = StrongStemClassMascA.unapply("úlfr", (SINGULAR, NOMINATIVE)).get

    assertEquals("úlfum", StrongStemClassMascA(root, (PLURAL, DATIVE)).strForm())
  }

  @Test
  def testStrongMascAStemInflectionForSong(): Unit = {

    val root = StrongStemClassMascA.unapply("sǫngr", (SINGULAR, NOMINATIVE)).get

    assertEquals("sǫng", StrongStemClassMascA(root, (SINGULAR, ACCUSATIVE)).strForm())

    assertEquals("sǫngvar", StrongStemClassMascA(root, (PLURAL, NOMINATIVE)).strForm())
    assertEquals("sǫngum", StrongStemClassMascA(root, (PLURAL, DATIVE)).strForm())
  }

  @Test
  def testStrongMascIStemInflectionForGuest(): Unit = {

    val nc = StrongStemClassMascI

    val root = nc.unapply("gestr", (SINGULAR, NOMINATIVE)).get

    assertEquals("gestum", nc(root, (PLURAL, DATIVE)).strForm())
  }

	@Test
	def testStrongAFemNounInflection(): Unit =
	{
	}
}
