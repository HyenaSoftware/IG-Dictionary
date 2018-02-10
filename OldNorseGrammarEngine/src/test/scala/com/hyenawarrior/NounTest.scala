package com.hyenawarrior

import com.hyenawarrior.NounTestAux.{diff, nonReversible}
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

    (SINGULAR, NOMINATIVE)  -> nonReversible("ǫr"),
    (SINGULAR, DATIVE)      -> nonReversible("ǫr"),
    (SINGULAR, GENITIVE)    -> "ǫrvar",

    (PLURAL, NOMINATIVE)  -> "ǫrvar",
    (PLURAL, GENITIVE)    -> "ǫrva",
    (PLURAL, DATIVE)      -> nonReversible("ǫrum")
  ))

  @Test
  def testStrongMascIForms(): Unit =  diff(StrongStemClassMascI, Map(

    (SINGULAR, NOMINATIVE)  -> "gestr",
    (SINGULAR, DATIVE)      -> "gest",
    (SINGULAR, GENITIVE)    -> "gests",

    (PLURAL, NOMINATIVE)  -> "gestir",
    (PLURAL, GENITIVE)    -> "gesta",
    (PLURAL, DATIVE)      -> "gestum"
  ))

  @Test
  def testStrongFeminineIForms(): Unit =  diff(StrongStemClassFeminineI, Map(

    (SINGULAR, NOMINATIVE)  -> "hǫll",
    (SINGULAR, DATIVE)      -> "hǫll",
    (SINGULAR, GENITIVE)    -> "hallar",

    (PLURAL, NOMINATIVE)  -> "hallir",
    (PLURAL, GENITIVE)    -> "halla",
    (PLURAL, DATIVE)      -> "hǫllum"
  ))

  @Test
  def testStrongMascRForms(): Unit =  diff(StrongStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "maðr",
    (SINGULAR, ACCUSATIVE)  -> "mann",
    (SINGULAR, GENITIVE)    -> "manns",

    (PLURAL, NOMINATIVE)  -> "menn",
    (PLURAL, GENITIVE)    -> "manna",
    (PLURAL, DATIVE)      -> "mǫnnum"
  ))

  @Test
  def testStrongFeminineRForms(): Unit =  diff(StrongStemClassFeminineR, Map(

    (SINGULAR, NOMINATIVE)  -> "strǫnd",
    (SINGULAR, GENITIVE)    -> "strandar",

    (PLURAL, NOMINATIVE)  -> "strendr",
    (PLURAL, GENITIVE)    -> "stranda",
    (PLURAL, DATIVE)      -> "strǫndum"
  ))

  @Test
  def testStrongMascUForms(): Unit =  diff(StrongStemClassMascU, Map(

    (SINGULAR, NOMINATIVE)  -> "vǫllr",
    (SINGULAR, ACCUSATIVE)  -> "vǫll",
    (SINGULAR, DATIVE)      -> "velli",
    (SINGULAR, GENITIVE)    -> "vallar",

    (PLURAL, NOMINATIVE)  -> "vellir",
    (PLURAL, DATIVE)      -> "vǫllum",
    (PLURAL, GENITIVE)    -> "vǫlla"
  ))

  // in/weak fem I-stem
  @Test
  def testWeakFemIForms(): Unit =  diff(WeakStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "gleði",
    (SINGULAR, ACCUSATIVE)  -> "gleði",

    (PLURAL, NOMINATIVE)  -> "gleðir",
    (PLURAL, GENITIVE)    -> "gleða",
    (PLURAL, DATIVE)      -> "gleðum"
  ))

  // on/jon weak feminine U-stem
  @Test
  def testWeakFemUForms(): Unit =  diff(WeakStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "saga",
    (SINGULAR, ACCUSATIVE)  -> "sǫgu",

    (PLURAL, NOMINATIVE)  -> "sǫgur",
    (PLURAL, GENITIVE)    -> "sagna"
  ))

  // an/jan weak masc A-stem
  @Test
  def testWeakMascAForms(): Unit =  diff(WeakStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "bogi",
    (SINGULAR, ACCUSATIVE)  -> "boga",

    (PLURAL, NOMINATIVE)  -> "bogar",
    (PLURAL, GENITIVE)    -> "boga",
    (PLURAL, DATIVE)      -> "bogum"
  ))

  // -nd/weak masc R-stem
  @Test
  def testWeakMascRForms(): Unit =  diff(WeakStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "bóndi",
    (SINGULAR, ACCUSATIVE)  -> "bónda",

    (PLURAL, NOMINATIVE)  -> "bœndr",
    (PLURAL, GENITIVE)    -> "bónda",
    (PLURAL, DATIVE)      -> "bóndum"
  ))

  // an/jan weak neuter
  @Test
  def testWeakNeuterForms(): Unit =  diff(WeakStemClassMascR, Map(

    (SINGULAR, NOMINATIVE)  -> "hjarta",
    (SINGULAR, ACCUSATIVE)  -> "hjarta",

    (PLURAL, NOMINATIVE)  -> "hjǫrtu",
    (PLURAL, GENITIVE)    -> "hjartna",
    (PLURAL, DATIVE)      -> "hjǫrtum"
  ))
}
