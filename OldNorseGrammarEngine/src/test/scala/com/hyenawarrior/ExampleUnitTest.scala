package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.FinitiveStrongVerbForm
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbForm.verbFrom
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{apply => _, unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.ACTIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import org.junit.Assert.{assertEquals, assertNotSame, assertSame}
import org.junit.Test

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest
{
  case class MockPoSForm(str: String) extends PoSForm
	{
    override def strForm: String = str
  }

  @Test
  def testActiveUmlaut(){

    val str = U_Umlaut("kalladu")

    assertEquals(Some("kǫlludu"), str)
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
		val stem = StrongVerbStem("brann", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

    val givenStrongVerb = FinitiveStrongVerbForm("brunnum", stem, Pronoun.PL_1, PAST, INDICATIVE, ACTIVE)

		val strongVerbResult = verbFrom(stem, Pronoun.SG_2, PAST, INDICATIVE, ACTIVE)
		assertEquals("brannt", strongVerbResult.strForm)

		val FinitiveStrongVerbForm(str, stemResult, pronoun, tense, INDICATIVE, ACTIVE) = strongVerbResult
		assertEquals("brannt", str)
		assertSame(Pronoun.SG_2, pronoun)
		assertSame(PAST, tense)
  }

	@Test
	def testStrongVerbInflection()
	{
		val stem = StrongVerbStem("brenn", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)
		val stemPS = StrongVerbStem("brann", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
		val stemPP = StrongVerbStem("brunn", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)

		// "brinn-" is incorrect but regular, "brenn-" is correct, but not regular
		assertEquals("brinnr", verbFrom(stem, Pronoun.SG_2, PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("brinna", 		verbFrom(stem, None, 					INFINITIVE, ACTIVE).strForm)
		assertEquals("brinnandi", verbFrom(stem, Some(PRESENT), PARTICIPLE, ACTIVE).strForm)

		assertEquals("brinn", 		verbFrom(stem, Pronoun.SG_1, PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("brinnr", 		verbFrom(stem, Pronoun.SG_2, PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("brinnr", 		verbFrom(stem, Pronoun.SG_3, PRESENT,	INDICATIVE, ACTIVE).strForm)

		assertEquals("brinnum", 	verbFrom(stem, Pronoun.PL_1,  PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("brinnið", 	verbFrom(stem, Pronoun.PL_2,	PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("brinna",		verbFrom(stem, Pronoun.PL_3,  PRESENT, INDICATIVE, ACTIVE).strForm)

		assertEquals("brann", 		verbFrom(stemPS, Pronoun.SG_1,  PAST, INDICATIVE, ACTIVE).strForm)
		assertEquals("brannt", 		verbFrom(stemPS, Pronoun.SG_2,	PAST, INDICATIVE, ACTIVE).strForm)
		assertEquals("brann", 		verbFrom(stemPS, Pronoun.SG_3,	PAST, INDICATIVE, ACTIVE).strForm)

		assertEquals("brunnum", 	verbFrom(stemPP, Pronoun.PL_1,	PAST, INDICATIVE, ACTIVE).strForm)
		assertEquals("brunnuð", 	verbFrom(stemPP, Pronoun.PL_2,	PAST, INDICATIVE, ACTIVE).strForm)
		assertEquals("brunnu",		verbFrom(stemPP, Pronoun.PL_3,  PAST, INDICATIVE, ACTIVE).strForm)
	}
}