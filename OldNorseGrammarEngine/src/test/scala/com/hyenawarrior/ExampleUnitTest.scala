package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb.verbFrom
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{apply => _, unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import org.junit.Assert.{assertEquals, assertNotSame, assertSame}
import org.junit.Test

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest
{
  case class MockPoS(str: String) extends PoS
	{
    override def strForm: String = str

    override def descriptorFlags: List[DescriptorFlag] = List()
  }

  @Test
  def testInactiveUmlaut()
	{
		// TODO: Word force apply transformation in every case.
		// Add an indicator to the transformators to avoid transformation when it's not needed
		//	e.g.: Word(mp, List(U_Umlaut)) vs. Word(mp, List(Explicit_U_Umlaut))

    val mp = MockPoS("kallada")

    val w = Word(mp)

    assertEquals("kallada", w.strForm())
  }

  @Test
  def testActiveUmlaut()
	{
    val mp = MockPoS("kalladu")

    val w = Word(mp)

    assertEquals("kǫlludu", w.strForm())
  }

	@Test
	def testAblautEnum()
	{
		val ABLAUT_1: AblautGrade = AblautGrade("x".replace('x', 'a'))
		val ABLAUT_2: AblautGrade = AblautGrade("y".replace('y', 'a'))

		assertEquals(ABLAUT_1, ABLAUT_2)
		assertNotSame(ABLAUT_1, ABLAUT_2)
	}

	@Test
  def testAblaut()
	{
		val stem = StrongVerbStem(Root("brenn"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

    val givenStrongVerb = FinitiveStrongVerb("brunnum", stem, Pronoun.PL_1, PAST, VerbModeEnum.INDICATIVE)

		val strongVerbResult = verbFrom(stem, Pronoun.SG_2, PAST, VerbModeEnum.INDICATIVE)

		val FinitiveStrongVerb(str, clazz, pronoun, tense, VerbModeEnum.INDICATIVE) = strongVerbResult
		assertEquals("brannt", str)
		assertSame(Pronoun.SG_2, pronoun)
		assertSame(PAST, tense)
  }

	@Test
	def testStrongVerbInflection()
	{
		val stem = StrongVerbStem(Root("brenn"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)
		val stemPS = StrongVerbStem(Root("brenn"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
		val stemPP = StrongVerbStem(Root("brenn"), VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)

		assertEquals("brennr", verbFrom(stem, Pronoun.SG_2, PRESENT, VerbModeEnum.INDICATIVE).strForm)

		assertEquals("brenna", 		verbFrom(stem, None, 					INFINITIVE).strForm)
		assertEquals("brennandi", verbFrom(stem, Some(PRESENT), PARTICIPLE).strForm)

		assertEquals("brenn", 		verbFrom(stem, Pronoun.SG_1, PRESENT, INDICATIVE).strForm)
		assertEquals("brennr", 		verbFrom(stem, Pronoun.SG_2, PRESENT, INDICATIVE).strForm)
		assertEquals("brennr", 		verbFrom(stem, Pronoun.SG_3, PRESENT,	INDICATIVE).strForm)

		assertEquals("brennum", 	verbFrom(stem, Pronoun.PL_1,  PRESENT, INDICATIVE).strForm)
		assertEquals("brennið", 	verbFrom(stem, Pronoun.PL_2,	PRESENT, INDICATIVE).strForm)
		assertEquals("brenna",		verbFrom(stem, Pronoun.PL_3,  PRESENT, INDICATIVE).strForm)

		assertEquals("brann", 		verbFrom(stemPS, Pronoun.SG_1,  PAST, INDICATIVE).strForm)
		assertEquals("brannt", 		verbFrom(stemPS, Pronoun.SG_2,	PAST, INDICATIVE).strForm)
		assertEquals("brann", 		verbFrom(stemPS, Pronoun.SG_3,	PAST, INDICATIVE).strForm)

		assertEquals("brunnum", 	verbFrom(stemPP, Pronoun.PL_1,	PAST, INDICATIVE).strForm)
		assertEquals("brunnuð", 	verbFrom(stemPP, Pronoun.PL_2,	PAST, INDICATIVE).strForm)
		assertEquals("brunnu",		verbFrom(stemPP, Pronoun.PL_3,  PAST, INDICATIVE).strForm)
	}
}