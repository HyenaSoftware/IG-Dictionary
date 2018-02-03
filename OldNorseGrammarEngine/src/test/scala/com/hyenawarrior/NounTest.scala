package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.Case.{ACCUSATIVE, DATIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{StrongStemClassMascA, StrongStemClassMascI}
import org.junit.Assert.assertEquals
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class NounTest {

  @Test
  def testStrongMascWaStem(): Unit = {

    val noun = Noun(StrongStemClassMascA, Map((SINGULAR, NOMINATIVE) -> "sǫngr"))

    assertEquals("sǫng", 		(noun nounForms SINGULAR -> ACCUSATIVE).strRepr)

    assertEquals("sǫngvar", (noun nounForms PLURAL -> NOMINATIVE).strRepr)
    assertEquals("sǫngum", 	(noun nounForms PLURAL -> DATIVE).strRepr)
  }

  @Test
  def testStrongMascIStem(): Unit = {

    val noun = Noun(StrongStemClassMascI, Map((SINGULAR, NOMINATIVE) -> "gestr"))

    assertEquals("gestum", (noun nounForms PLURAL -> DATIVE).strRepr)
  }

	@Test
	def testStrongMascAForms(): Unit =
	{
    val noun = Noun(StrongStemClassMascA, Map((SINGULAR, NOMINATIVE) -> "úlfr"))

		assertEquals("úlf", 	(noun nounForms SINGULAR -> ACCUSATIVE).strRepr)

		assertEquals("úlfar",	(noun nounForms PLURAL   -> NOMINATIVE).strRepr)
		assertEquals("úlfa",	(noun nounForms PLURAL   -> ACCUSATIVE).strRepr)
		assertEquals("úlfum",	(noun nounForms PLURAL   -> DATIVE).strRepr)
	}
}
