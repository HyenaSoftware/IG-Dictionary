package com.hyenawarrior

import com.hyenawarrior.NounTestAux.diff
import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses._
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
    (SINGULAR, GENITIVE) -> "ríkis",

    (PLURAL, ACCUSATIVE) -> "ríki",
    (PLURAL, GENITIVE) -> "ríkja",
    (PLURAL, DATIVE) -> "ríkjum"
  ))

  @Test
  def testStrongNeuterAForms2(): Unit =  diff(StrongStemClassNeuter, Map(

    (SINGULAR, NOMINATIVE) -> "land",
    (SINGULAR, GENITIVE) -> "lands",

    (PLURAL, ACCUSATIVE) -> "lǫnd",
    (PLURAL, GENITIVE) -> "landa",
    (PLURAL, DATIVE) -> "lǫndum"
  ))

  @Test
  def testStrongNeuterWaForms(): Unit =  diff(StrongStemClassNeuter, Map(

    (SINGULAR, NOMINATIVE) -> "hǫgg",
    (SINGULAR, ACCUSATIVE) -> "hǫgg",
    (SINGULAR, DATIVE) -> "hǫggvi",

    (PLURAL, NOMINATIVE) -> "hǫgg",
    (PLURAL, GENITIVE) -> "hǫggva",
    (PLURAL, DATIVE) -> "hǫggum"
  ))

  @Test
  def testStrongFeminineJoFormsLong(): Unit =  diff(StrongStemClassFeminineA1, Map(

    (SINGULAR, NOMINATIVE) -> "heiðr",
    (SINGULAR, GENITIVE) -> "heiðar",

    (PLURAL, NOMINATIVE) -> "heiðar",
    (PLURAL, GENITIVE) -> "heiða",
    (PLURAL, DATIVE) -> "heiðum"
  ))

  @Test
  def testStrongFeminineOForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    (SINGULAR, NOMINATIVE) -> "grǫf",
    (SINGULAR, GENITIVE) -> "grafar",

    (PLURAL, NOMINATIVE) -> "grafar",
    (PLURAL, GENITIVE) -> "grafa",
    (PLURAL, DATIVE) -> "grǫfum"
  ))

  @Test
  def testStrongFeminineJoForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    (SINGULAR, NOMINATIVE) -> "ey",
    (SINGULAR, DATIVE) -> "eyju",
    (SINGULAR, GENITIVE) -> "eyjar",

    (PLURAL, NOMINATIVE) -> "eyjar",
    (PLURAL, GENITIVE) -> "eyja",
    (PLURAL, DATIVE) -> "eyjum"
  ))

  @Test
  def testStrongFeminineWoForms(): Unit =  diff(StrongStemClassFeminineA2, Map(

    (SINGULAR, NOMINATIVE) -> "ǫr",
    (SINGULAR, DATIVE) -> "ǫr",
    (SINGULAR, GENITIVE) -> "ǫrvar",

    (PLURAL, NOMINATIVE) -> "ǫrvar",
    (PLURAL, GENITIVE) -> "ǫrva",
    (PLURAL, DATIVE) -> "ǫrum"
  ))
}
