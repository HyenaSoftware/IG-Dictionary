package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{apply => _, unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass._
import org.junit.Assert.{assertEquals, assertNotSame, assertSame, assertTrue}
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest
{
	private def convertTo(sv: StrongVerb, fp: FinitiveVerbDesc) =
	{
		StrongVerbStemClasses.convertTo(sv, sv.verbClassDesc.ablaut.asInstanceOf[StaticAblaut], fp)
	}

	private def convertTo(sv: StrongVerb, nfp: NonFinitiveVerbType) =
	{
		StrongVerbStemClasses.convertTo(sv, sv.verbClassDesc.ablaut.asInstanceOf[StaticAblaut], nfp)
	}


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
		val vcd = verbs.getDescOfStrongVerbClassFor(VerbClassEnum.STRONG_3RD_CLASS)

		assertTrue(vcd.isDefined)

    val givenStrongVerb = FinitiveStrongVerb("brunnum", vcd.head, Pronoun.PL_1, PAST, VerbModeEnum.INDICATIVE)

		val finitiveParams = (Pronoun.SG_2, INDICATIVE, PAST)

    val strongVerbResult = convertTo(givenStrongVerb, finitiveParams)

		strongVerbResult match {

			case Some(FinitiveStrongVerb(str, clazz, pronoun, tense, VerbModeEnum.INDICATIVE)) =>
				assertEquals("brannt", str)
				assertEquals(vcd.head, clazz)
				assertSame(Pronoun.SG_2, pronoun)
				assertSame(PAST, tense)

			case _ => Assert.fail()
		}
  }

	@Test
	def testStrongVerbInflection()
	{
		val vcd = verbs.getDescOfStrongVerbClassFor(VerbClassEnum.STRONG_3RD_CLASS)

		assertTrue(vcd.isDefined)

		val baseVerb = FinitiveStrongVerb("brennr", vcd.head, Pronoun.SG_2, PRESENT, VerbModeEnum.INDICATIVE)

		assertEquals("brenna", 		convertTo(baseVerb, NonFinitiveVerbType.INFINITIVE).map(_.strForm).orNull)
		assertEquals("brennandi", convertTo(baseVerb, NonFinitiveVerbType.PRESENT_PARTICIPLE).map(_.strForm).orNull)

		assertEquals("brenn", convertTo(baseVerb, (Pronoun.SG_1, 				INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennr", convertTo(baseVerb, (Pronoun.SG_2,				INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennr", convertTo(baseVerb, (Pronoun.SG_3_MASC,	INDICATIVE, PRESENT)).map(_.strForm).orNull)

		assertEquals("brennum", convertTo(baseVerb, (Pronoun.PL_1,			INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brennið", convertTo(baseVerb, (Pronoun.PL_2,			INDICATIVE, PRESENT)).map(_.strForm).orNull)
		assertEquals("brenna",	convertTo(baseVerb, (Pronoun.PL_3_MASC, INDICATIVE, PRESENT)).map(_.strForm).orNull)

		assertEquals("brann", convertTo(baseVerb, (Pronoun.SG_1, 				INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brannt", convertTo(baseVerb, (Pronoun.SG_2,		 		INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brann", convertTo(baseVerb, (Pronoun.SG_3_MASC,		INDICATIVE, PAST)).map(_.strForm).orNull)

		assertEquals("brunnum", convertTo(baseVerb, (Pronoun.PL_1,			INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brunnuð", convertTo(baseVerb, (Pronoun.PL_2,			INDICATIVE, PAST)).map(_.strForm).orNull)
		assertEquals("brunnu",	convertTo(baseVerb, (Pronoun.PL_3_MASC, INDICATIVE, PAST)).map(_.strForm).orNull)
	}
}