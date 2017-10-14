package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum.STRONG_1ST_CLASS
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root}
import org.junit.Assert._
import org.junit.{Assert, Test}

import scala.collection.immutable.Map

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class VerbTest
{
	@Test
	def testStrong6VerbInflection()
	{
		val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)
		val svStemPS = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
		val svStemPP = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
		val svStemPF = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PERFECT_STEM)

		assertEquals("taka",		verbFrom(svStemPR, None, INFINITIVE).strForm)

		assertEquals("takinn", 	verbFrom(svStemPF, Some(PAST), 		PARTICIPLE).strForm)
		assertEquals("takandi", verbFrom(svStemPR, Some(PRESENT), PARTICIPLE).strForm)

		assertEquals("tek", 		verbFrom(svStemPR, Pronoun.SG_1, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_2,			PRESENT,	INDICATIVE).strForm)
		assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_3_MASC, PRESENT,	INDICATIVE).strForm)

		assertEquals("tökum", 	verbFrom(svStemPR, Pronoun.PL_1, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("takið", 	verbFrom(svStemPR, Pronoun.PL_2, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("taka",		verbFrom(svStemPR, Pronoun.PL_3_MASC,	PRESENT,	INDICATIVE).strForm)

		assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_1, 			PAST, 		INDICATIVE).strForm)
		assertEquals("tókt" /*"tókst"*/, verbFrom(svStemPS, Pronoun.SG_2, PAST,	INDICATIVE).strForm)
		assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_3_MASC, PAST,			INDICATIVE).strForm)

		assertEquals("tókum",		verbFrom(svStemPP, Pronoun.PL_1, 			PAST,			INDICATIVE).strForm)
		assertEquals("tókuð",		verbFrom(svStemPP, Pronoun.PL_2,			PAST,			INDICATIVE).strForm)
		assertEquals("tóku",		verbFrom(svStemPP, Pronoun.PL_3_MASC, PAST,			INDICATIVE).strForm)
	}

  @Test
  def testVerbGenerator(): Unit = try {

    val vfSg1 = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1))
    val vfSg2 = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2))

    val verb = StrongVerbContext(STRONG_1ST_CLASS, Map(vfSg2 -> "beit"))

    val verbForm = verb.verbForms(vfSg1)

    assertEquals(verbForm.strForm, "bei")

  } catch {

    case e: RuntimeException => throw e
  }

	@Test
	def sanityCheck() {

		val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)

		try {

			verbFrom(svStemPR, Some(PAST), 		PARTICIPLE)

			Assert.fail("Past participle should not be sonstructible from present stem")
		}
		catch {
			case e: RuntimeException => ()
		}
	}
}
