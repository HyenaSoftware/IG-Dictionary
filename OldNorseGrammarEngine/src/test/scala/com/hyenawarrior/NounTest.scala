package com.hyenawarrior

import com.hyenawarrior.NounTestAux.diff
import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{StrongStemClassMascA, StrongStemClassMascI, StrongStemClassNeuter}
import org.junit.Assert._
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
	def testStrongMascAForms(): Unit = {

    val noun = Noun(StrongStemClassMascA, Map((SINGULAR, NOMINATIVE) -> "úlfr"))

		assertEquals("úlf", 	(noun nounForms SINGULAR -> ACCUSATIVE).strRepr)

		assertEquals("úlfar",	(noun nounForms PLURAL   -> NOMINATIVE).strRepr)
		assertEquals("úlfa",	(noun nounForms PLURAL   -> ACCUSATIVE).strRepr)
		assertEquals("úlfum",	(noun nounForms PLURAL   -> DATIVE).strRepr)
	}

  @Test
  def testStrongNeuterAForms(): Unit =  diff(StrongStemClassNeuter, Map(

    (SINGULAR, NOMINATIVE) -> "kné",
    (SINGULAR, GENITIVE)   -> "knés",

    (PLURAL,   NOMINATIVE) -> "kné",
    (PLURAL,   GENITIVE)   -> "knjá",
    (PLURAL,   DATIVE)     -> "knjám"
    //(PLURAL,   DATIVE)     -> "knjóm"
  ))

  @Test
  def testStrongNeuterJaForms(): Unit =  diff(StrongStemClassNeuter, Map(

    (SINGULAR, NOMINATIVE) -> "ríki",
    (SINGULAR -> GENITIVE) -> "ríkis",

    (PLURAL -> ACCUSATIVE) -> "ríki",
    (PLURAL -> GENITIVE) -> "ríkja",
    (PLURAL -> DATIVE) -> "ríkjum"
  ))

  @Test
  def testStrongNeuterAForms2(): Unit =  diff(StrongStemClassNeuter, Map(

    (SINGULAR, NOMINATIVE) -> "land",
    (SINGULAR -> GENITIVE) -> "lands",

    (PLURAL -> ACCUSATIVE) -> "lǫnd",
    (PLURAL -> GENITIVE) -> "landa",
    (PLURAL -> DATIVE) -> "lǫndum"
  ))

  @Test
  def testStrongNeuterWaForms(): Unit =  {

    val noun = Noun(StrongStemClassNeuter, Map((SINGULAR, NOMINATIVE) -> "hǫgg"))

    assertEquals("hǫgg", 	(noun nounForms SINGULAR -> ACCUSATIVE).strRepr)

    assertEquals("hǫgg",	  (noun nounForms PLURAL   -> NOMINATIVE).strRepr)
    assertEquals("hǫggva",	(noun nounForms PLURAL   -> GENITIVE).strRepr)
    assertEquals("hǫggum",	(noun nounForms PLURAL   -> DATIVE).strRepr)
  }
}
