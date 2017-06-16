package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{apply => _, unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses.convertTo
import org.junit.Assert.{assertEquals, assertNotSame, assertSame}
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest {

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

    assertEquals("kölludu", w.strForm())
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
    val givenStrongVerb = FinitiveStrongVerb("brunnum", VerbClassEnum.STRONG_3RD_CLASS, Pronoun.PL_1, PAST)

		val finitiveParams = (Pronoun.SG_2, INDICATIVE, PAST)

    val strongVerbResult = convertTo(givenStrongVerb, Left(finitiveParams))

		strongVerbResult match {

			case Some(FinitiveStrongVerb(str, clazz, pronoun, tense)) =>
				assertEquals("brannt", str)
				assertSame(VerbClassEnum.STRONG_3RD_CLASS, clazz)
				assertSame(Pronoun.SG_2, pronoun)
				assertSame(PAST, tense)

			case _ => Assert.fail()
		}
  }

	@Test
	def testStrongVerbInflection()
	{
		val baseVerb = FinitiveStrongVerb("brennr", VerbClassEnum.STRONG_3RD_CLASS, Pronoun.SG_2, PRESENT)

		assertEquals("brenna", 		convertTo(baseVerb, Right(NonFinitiveVerbType.INFINITIVE)).map(_.strForm).orNull)
		assertEquals("brennandi", convertTo(baseVerb, Right(NonFinitiveVerbType.PRESENT_PARTICIPLE)).map(_.strForm).orNull)

		assertEquals("brenn", convertTo(baseVerb, Left(Pronoun.SG_1, 				INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennr", convertTo(baseVerb, Left(Pronoun.SG_2,				INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennr", convertTo(baseVerb, Left(Pronoun.SG_3_MASC,	INDICATIVE, PRESENT)).map(_.strForm).orNull)

		assertEquals("brennum", convertTo(baseVerb, Left(Pronoun.PL_1,			INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennið", convertTo(baseVerb, Left(Pronoun.PL_2,			INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brenna",	convertTo(baseVerb, Left(Pronoun.PL_3_MASC, INDICATIVE, PRESENT)).map(_.strForm).orNull)

		assertEquals("brann", convertTo(baseVerb, Left(Pronoun.SG_1, 				INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brannt", convertTo(baseVerb, Left(Pronoun.SG_2,		 		INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brann", convertTo(baseVerb, Left(Pronoun.SG_3_MASC,		INDICATIVE, PAST)).map(_.strForm).orNull)

		assertEquals("brunnum", convertTo(baseVerb, Left(Pronoun.PL_1,			INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brunnuð", convertTo(baseVerb, Left(Pronoun.PL_2,			INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brunnu",	convertTo(baseVerb, Left(Pronoun.PL_3_MASC, INDICATIVE, PAST)).map(_.strForm).orNull)
	}
}